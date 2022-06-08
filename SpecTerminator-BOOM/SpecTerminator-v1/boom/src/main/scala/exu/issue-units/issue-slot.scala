//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Slot Logic              RISCV处理器issue槽逻辑
//--------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Note: stores (and AMOs) are "broken down" into 2 uops, but stored within a single issue-slot.
// 注意:商店(和AMOs)被“分解”为2个uops，但存储在一个发行槽中。
// TODO XXX make a separate issueSlot for MemoryIssueSlots, and only they break apart stores.
// TODO Disable ldspec for FP queue.

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters

import boom.common._
import boom.util._
import FUConstants._

/**
 * IO bundle to interact with Issue slot                         连接issue slot的io bundle
 *
 * @param numWakeupPorts number of wakeup ports for the slot     槽的唤醒端口数目
 */
class IssueSlotIO(val numWakeupPorts: Int)(implicit p: Parameters) extends BoomBundle
{
  val valid         = Output(Bool())
  val will_be_valid = Output(Bool()) // TODO code review, do we need this signal so explicitely?  做代码复查，我们需要这个明确的信号吗?
  val request       = Output(Bool())
  val request_hp    = Output(Bool())
  val grant         = Input(Bool())   //承认，同意，合法

  val brupdate        = Input(new BrUpdateInfo())
  val kill          = Input(Bool()) // pipeline flush          流水线刷新
  val clear         = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)     条目被转移到其他地方(与grant不相互排斥)
  val ldspec_miss   = Input(Bool()) // Previous cycle's speculative load wakeup was mispredicted.          先前周期的投机load唤醒是错误的预测。

  val wakeup_ports  = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))
  val spec_ld_wakeup = Flipped(Vec(memWidth, Valid(UInt(width=maxPregSz.W))))
  val in_uop        = Flipped(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!             如果有效，这将覆盖一个条目!
  val out_uop   = Output(new MicroOp()) // the updated slot uop; will be shifted upwards in a collasping queue.    更新的槽uop;将在collasping队列中向上移动。
  val uop           = Output(new MicroOp()) // the current Slot's uop. Sent down the pipeline when issued.         当前槽的uop。发出时通过管道发送。

  val risk_pdst = Output(UInt(32.W))
  val risk_debug_inst = Output(UInt(32.W))
  val risk_rob_idx = Output(UInt(32.W))
  val idle_cycles = Input(UInt(32.W))
  val return_issue = Input(Bool())

  val risk_table = Input(Vec(numIntPhysRegs,Bool()))
  val fp_risk_table = Input(Vec(numFpPhysRegs,Bool()))
  val st_risk_table = Input(Vec(numIntPhysRegs,Bool()))
  val st_fp_risk_table = Input(Vec(numFpPhysRegs,Bool()))


  val debug = {
    val result = new Bundle {
      val p1 = Bool()
      val p2 = Bool()
      val p3 = Bool()
      val ppred = Bool()
      val state = UInt(width=2.W)
    }
    Output(result)
  }
}

/**
 * Single issue slot. Holds a uop within the issue queue                 单个issue slot。在issue队列中持有uop
 *
 * @param numWakeupPorts number of wakeup ports
 */
class IssueSlot(val numWakeupPorts: Int)(implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
{
  val io = IO(new IssueSlotIO(numWakeupPorts))

  // slot invalid?
  // slot is valid, holding 1 uop
  // slot is valid, holds 2 uops (like a store)
  // 槽无效?
  // 插槽有效，持有1个uop
  // 插槽有效，容纳2个uops(像store)
  def is_invalid = state === s_invalid
  def is_valid = state =/= s_invalid

  val next_state      = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)    这个槽的下一个状态(可能会移动到一个新的槽)
  val next_uopc       = Wire(UInt()) // the next uopc of this slot (which might then get moved to a new slot)     此槽的下一个uopc(随后可能移动到新的槽)
  val next_lrs1_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot) 此槽的下一个reg类型(随后可能会移动到新的槽)
  val next_lrs2_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot) 此槽的下一个reg类型(随后可能会移动到新的槽)

  val state = RegInit(s_invalid)
  val p1    = RegInit(false.B)
  val p2    = RegInit(false.B)
  val p3    = RegInit(false.B)
  val ppred = RegInit(false.B)

  // Poison if woken up by speculative load.
  // Poison lasts 1 cycle (as ldMiss will come on the next cycle).
  // SO if poisoned is true, set it to false!
  // 如果被投机性load唤醒，会中毒。
  // 毒药持续1个周期(ldMiss将在下一个周期出现)。
  // 所以如果中毒为真，将其设置为假!
  val p1_poisoned = RegInit(false.B)
  val p2_poisoned = RegInit(false.B)
  p1_poisoned := false.B
  p2_poisoned := false.B
  val next_p1_poisoned = Mux(io.in_uop.valid, io.in_uop.bits.iw_p1_poisoned, p1_poisoned)
  val next_p2_poisoned = Mux(io.in_uop.valid, io.in_uop.bits.iw_p2_poisoned, p2_poisoned)
  //iw_p1_poisoned、iw_p2_poisoned：操作数1或2是否被负载推测性唤醒?
  //只有整数操作数被推测地唤醒，所以我们可以忽略p3。

  val slot_uop = RegInit(NullMicroOp)
  val next_uop = Mux(io.in_uop.valid, io.in_uop.bits, slot_uop)

  //-----------------------------------------------------------------------------
  // next slot state computation
  // compute the next state for THIS entry slot (in a collasping queue, the
  // current uop may get moved elsewhere, and a new uop can enter
  // 下一个槽状态计算
  // 计算此入口槽的下一个状态(在collasping队列中，当前uop可能被移动到其他地方，新的uop可以进入

  when (io.kill) {
    state := s_invalid
  } .elsewhen (io.in_uop.valid) {
    state := io.in_uop.bits.iw_state
  } .elsewhen (io.clear) {
    state := s_invalid
  } .otherwise {
    state := next_state
  }

  //-----------------------------------------------------------------------------
  // "update" state
  // compute the next state for the micro-op in this slot. This micro-op may
  // be moved elsewhere, so the "next_state" travels with it.
  //“更新”状态
  //计算此槽中微操作的下一个状态。这个微op可能被移动到其他地方，因此“next_state”会随它一起移动。

  // defaults
  next_state := state
  next_uopc := slot_uop.uopc
  next_lrs1_rtype := slot_uop.lrs1_rtype
  next_lrs2_rtype := slot_uop.lrs2_rtype

  //grant:承认，同意，合法
  when (io.kill) {
    next_state := s_invalid
  } .elsewhen ((io.grant && (state === s_valid_1)) ||
    (io.grant && (state === s_valid_2) && p1 && p2 && ppred)) {
    // try to issue this uop.                 尝试发出这个uop。
    when (!(io.ldspec_miss && (p1_poisoned || p2_poisoned))) {
      next_state := s_invalid
    }
  } .elsewhen (io.grant && (state === s_valid_2)) {
    when (!(io.ldspec_miss && (p1_poisoned || p2_poisoned))) {
      next_state := s_valid_1
      when (p1) {
        slot_uop.uopc := uopSTD
        next_uopc := uopSTD
        slot_uop.lrs1_rtype := RT_X
        next_lrs1_rtype := RT_X
      } .otherwise {
        slot_uop.lrs2_rtype := RT_X
        next_lrs2_rtype := RT_X
      }
    }
  }

  when (io.in_uop.valid) {
    slot_uop := io.in_uop.bits
    assert (is_invalid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
  }

  // Wakeup Compare Logic             唤醒比较逻辑

  // these signals are the "next_p*" for the current slot's micro-op.
  // they are important for shifting the current slot_uop up to an other entry.
  // 这些信号是当前插槽的微操作的“next_p*”。
  // 它们对于将当前slot_uop移动到其他条目非常重要。
  val next_p1 = WireInit(p1)
  val next_p2 = WireInit(p2)
  val next_p3 = WireInit(p3)
  val next_ppred = WireInit(ppred)

  when (io.in_uop.valid) {
    p1 := !(io.in_uop.bits.prs1_busy)
    p2 := !(io.in_uop.bits.prs2_busy)
    p3 := !(io.in_uop.bits.prs3_busy)
    ppred := !(io.in_uop.bits.ppred_busy)
  }
  
  
  //不能为prs1=x0设置中毒位
  when (io.ldspec_miss && next_p1_poisoned) {
    assert(next_uop.prs1 =/= 0.U, "Poison bit can't be set for prs1=x0!")
    p1 := false.B
  }
  //不能为prs2=x0设置中毒位
  when (io.ldspec_miss && next_p2_poisoned) {
    assert(next_uop.prs2 =/= 0.U, "Poison bit can't be set for prs2=x0!")
    p2 := false.B
  }


  val return_issue = WireInit(false.B)
  return_issue := io.return_issue

  //when(return_issue) {
    io.risk_pdst := 0.U
    io.risk_debug_inst := 0.U
    io.risk_rob_idx := 0.U
    //printf(p"clear-risk-debug-inst\n ")
  //}

   /*for (i <- 0 until numWakeupPorts) {
    when ((io.wakeup_ports(i).valid && (io.wakeup_ports(i).bits.pdst === next_uop.prs1)) || (io.wakeup_ports(i).valid && (io.wakeup_ports(i).bits.pdst === next_uop.prs2))) {
    when(io.wakeup_ports(i).bits.pdst === 39.U) {
      printf(p" i=${i}")
       printf(p" cycles=${io.idle_cycles} ")
      printf(" wakeup-39-2 \n")
    }  
    when(io.wakeup_ports(i).bits.pdst === 17.U) {
      printf(p" i=${i}")
       printf(p" cycles=${io.idle_cycles} ")
      printf(" wakeup-17-2 \n")
    } 
    }
    }*/

  for (i <- 0 until numWakeupPorts) {
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs1)) {
     /* when(io.wakeup_ports(i).bits.debug_inst === 0xf8843703L.U && next_uop.debug_pc === 0x80001318L.U){
        val xx = io.wakeup_ports(i).bits.risk
        val yy = io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)
        printf(p"\nxx io.wakeup_ports(i).bits.risk =${xx} issued")
        printf(p"yy =${yy} ")
        printf(p"next_uop_mask =${next_uop.br_mask} ")
        printf(p"next_uop_rob_idx=${next_uop.rob_idx} ")
        printf(p" cycles=${io.idle_cycles} ")
        printf(" find 0xf8843703L in issue-slot 0x80001318L \n")
      }
      when(io.wakeup_ports(i).bits.debug_inst === 0x0ff00793L.U && next_uop.debug_pc === 0x80001318L.U){
        val xx = io.wakeup_ports(i).bits.risk
        val yy = io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)
        printf(p"\nxx11 io.wakeup_ports(i).bits.risk =${xx} issued")
        printf(p"yy =${yy} ")
        printf(p"next_uop_mask =${next_uop.br_mask} ")
        printf(p"next_uop_rob_idx=${next_uop.rob_idx} ")
        printf(p" cycles=${io.idle_cycles} ")
        printf(" find 0x0ff00793L in issue-slot 0x80001318L \n")
      }
       when(io.wakeup_ports(i).bits.debug_inst(15,0) === 0x0785L.U && next_uop.debug_pc === 0x80001318L.U){
        val xx = io.wakeup_ports(i).bits.risk
        val yy = io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)
        printf(p"\nxx11 io.wakeup_ports(i).bits.risk =${xx} issued")
        printf(p"yy =${yy} ")
        printf(p"next_uop_mask =${next_uop.br_mask} ")
        printf(p"next_uop_rob_idx=${next_uop.rob_idx} ")
        printf(p" cycles=${io.idle_cycles} ")
        printf(" find 0x0785L in issue-slot  0x80001318L\n")
      }
       when(io.wakeup_ports(i).bits.debug_inst === 0xf8f43423L.U && next_uop.debug_pc === 0x80001318L.U){
        val xx = io.wakeup_ports(i).bits.risk
        val yy = io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)
        printf(p"\nxx11 io.wakeup_ports(i).bits.risk =${xx} issued")
        printf(p"yy =${yy} ")
        printf(p"next_uop_mask =${next_uop.br_mask} ")
        printf(p"next_uop_rob_idx=${next_uop.rob_idx} ")
        printf(p" cycles=${io.idle_cycles} ")
        printf(" find 0xf8f43423L in issue-slot 0x80001318L \n")
      }
       when(io.wakeup_ports(i).bits.debug_inst === 0xf8843783L.U && next_uop.debug_pc === 0x80001318L.U){
        val xx = io.wakeup_ports(i).bits.risk
        val yy = io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)
        printf(p"\nxx11 io.wakeup_ports(i).bits.risk =${xx} issued")
        printf(p"yy =${yy} ")
        printf(p"next_uop_mask =${next_uop.br_mask} ")
        printf(p"next_uop_rob_idx=${next_uop.rob_idx} ")
        printf(p" cycles=${io.idle_cycles} ")
        printf(" find f8843783 in issue-slot  0x80001318L\n")
      }*/
      when(io.wakeup_ports(i).bits.risk =/= 0.U && io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)){
      //when(io.wakeup_ports(i).bits.risk =/= 0.U && io.wakeup_ports(i).bits.pdst =/= 0.U && next_uop.uses_ldq && io.wakeup_ports(i).bits.debug_inst(15,0) === 0x97baL.U && next_uop.debug_inst === 0x0007c783L.U){
        //io.risk_pdst := io.wakeup_ports(i).bits.pdst
        ////io.risk_debug_inst := io.wakeup_ports(i).bits.debug_inst
        //io.risk_rob_idx := io.wakeup_ports(i).bits.risk_rob_idx
       /* printf(p" risk_pdst=${io.wakeup_ports(i).bits.pdst} ")
        //printf(" risk_debug_inst:(0x%x)", io.wakeup_ports(i).bits.debug_inst(31,0))
        printf(" next_uop:(0x%x)", next_uop.debug_inst)
        printf(p" cycles=${io.idle_cycles} ")
        printf(" start-risk-debug-inst1 \n")*/

        //p1 := false.B
        p1 := true.B
      } .otherwise {
        p1 := true.B
        //
      }
    }

    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs2)) {
      //next_uop(load)拒绝带有risk标签的uop的唤醒
      when(io.wakeup_ports(i).bits.risk =/= 0.U && io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr)){
      //  when(io.wakeup_ports(i).bits.risk =/= 0.U && io.wakeup_ports(i).bits.pdst =/= 0.U && next_uop.uses_ldq && io.wakeup_ports(i).bits.debug_inst(15,0) === 0x97baL.U && next_uop.debug_inst === 0x0007c783L.U){
        //io.risk_pdst := io.wakeup_ports(i).bits.pdst
        //io.risk_debug_inst := io.wakeup_ports(i).bits.debug_inst
        //io.risk_rob_idx := io.wakeup_ports(i).bits.risk_rob_idx
       /* when(next_uop.fp_val){
             printf(p" \nerror: a float wakeup2\n")
        } */
        /*printf(p" risk_pdst=${io.risk_pdst(15,0)} ")
        printf(" risk_debug_inst:(0x%x)", io.risk_debug_inst(15,0))
        printf(p" cycles=${io.idle_cycles} ")
        printf(" start-risk-debug-inst2 \n")*/

        //p2 := false.B
        p2 := true.B
      } .otherwise {
        p2 := true.B
        //p2 := Mux(io.wakeup_ports(i).valid && (io.wakeup_ports(i).bits.pdst === next_uop.prs2) && io.wakeup_ports(i).bits.risk =/= 0.U && io.wakeup_ports(i).bits.pdst =/= 0.U && (next_uop.is_br || next_uop.is_jalr), false.B, true.B)
      }
    }

    
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs3)) {
      p3 := true.B
    }
  }
  
  when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === next_uop.ppred) {
    ppred := true.B
  }


  
  //load到x0时不应该推测性唤醒其他指令            ？重点注意  不应该推测性唤醒其他指令
  for (w <- 0 until memWidth) {
    assert (!(io.spec_ld_wakeup(w).valid && io.spec_ld_wakeup(w).bits === 0.U),
      "Loads to x0 should never speculatively wakeup other instructions")
  }

  // TODO disable if FP IQ.
  for (w <- 0 until memWidth) {
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits === next_uop.prs1 &&
      next_uop.lrs1_rtype === RT_FIX) {
      p1 := true.B
      p1_poisoned := true.B
      assert (!next_p1_poisoned)
    }
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits === next_uop.prs2 &&
      next_uop.lrs2_rtype === RT_FIX) {
      p2 := true.B
      p2_poisoned := true.B
      assert (!next_p2_poisoned)
    }
  }


  // Handle branch misspeculations              //处理分支的错误猜测
  val next_br_mask = GetNewBrMask(io.brupdate, slot_uop)

  // was this micro-op killed by a branch? if yes, we can't let it be valid if
  // we compact it into an other entry                   //这个微操作是被分支杀死吗？如果是，我们不能让他有效，如果我们将其压缩到另一个条目中
  when (IsKilledByBranch(io.brupdate, slot_uop)) {
    next_state := s_invalid
  }

  when (!io.in_uop.valid) {
    slot_uop.br_mask := next_br_mask
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //-------------------------------------------------------------
  // Request Logic         请求的逻辑
  
  val prs1_risk1 = Mux(next_uop.lrs1_rtype === RT_FIX, io.risk_table(next_uop.prs1), Mux(next_uop.lrs1_rtype === RT_FLT, io.fp_risk_table(next_uop.prs1), false.B) )
  val prs2_risk1 = Mux(next_uop.lrs2_rtype === RT_FIX, io.risk_table(next_uop.prs2), Mux(next_uop.lrs2_rtype === RT_FLT, io.fp_risk_table(next_uop.prs2), false.B) )
  val prs1_risk_st1 = Mux(next_uop.lrs1_rtype === RT_FIX, io.st_risk_table(next_uop.prs1), Mux(next_uop.lrs1_rtype === RT_FLT, io.st_fp_risk_table(next_uop.prs1), false.B) )
  val prs2_risk_st1 = Mux(next_uop.lrs2_rtype === RT_FIX, io.st_risk_table(next_uop.prs2), Mux(next_uop.lrs2_rtype === RT_FLT, io.st_fp_risk_table(next_uop.prs2), false.B) ) 
  val all_risk1 = (next_uop.is_br || next_uop.is_jalr) && (prs1_risk1 || prs2_risk1 || prs1_risk_st1 || prs2_risk_st1)


  io.request := is_valid && p1 && p2 && p3 && ppred && !io.kill // && !all_risk1

  val high_priority = slot_uop.is_br || slot_uop.is_jal || slot_uop.is_jalr
  io.request_hp := io.request && high_priority

  when (state === s_valid_1) {
    io.request := p1 && p2 && p3 && ppred && !io.kill //&& !all_risk1
  } .elsewhen (state === s_valid_2) {
    io.request := (p1 || p2) && ppred && !io.kill //&& !all_risk1
  } .otherwise {
    io.request := false.B
  }

  //assign outputs                / /分配输出
  io.valid := is_valid
  io.uop := slot_uop
  io.uop.iw_p1_poisoned := p1_poisoned
  io.uop.iw_p2_poisoned := p2_poisoned

  // micro-op will vacate due to grant.               micro-op将会因完成而离开。
  val may_vacate = io.grant && ((state === s_valid_1) || (state === s_valid_2) && p1 && p2 && ppred)
  val squash_grant = io.ldspec_miss && (p1_poisoned || p2_poisoned)
  io.will_be_valid := is_valid && !(may_vacate && !squash_grant)

  io.out_uop            := slot_uop
  io.out_uop.iw_state   := next_state
  io.out_uop.uopc       := next_uopc
  io.out_uop.lrs1_rtype := next_lrs1_rtype
  io.out_uop.lrs2_rtype := next_lrs2_rtype
  io.out_uop.br_mask    := next_br_mask
  io.out_uop.prs1_busy  := !p1
  io.out_uop.prs2_busy  := !p2
  io.out_uop.prs3_busy  := !p3
  io.out_uop.ppred_busy := !ppred
  io.out_uop.iw_p1_poisoned := p1_poisoned
  io.out_uop.iw_p2_poisoned := p2_poisoned

  when (state === s_valid_2) {
    when (p1 && p2 && ppred) {
      ; // send out the entire instruction as one uop          将整个指令作为一个uop发送
    } .elsewhen (p1 && ppred) {
      io.uop.uopc := slot_uop.uopc
      io.uop.lrs2_rtype := RT_X
    } .elsewhen (p2 && ppred) {
      io.uop.uopc := uopSTD
      io.uop.lrs1_rtype := RT_X
    }
  }

  // debug outputs
  io.debug.p1 := p1
  io.debug.p2 := p2
  io.debug.p3 := p3
  io.debug.ppred := ppred
  io.debug.state := state
}
