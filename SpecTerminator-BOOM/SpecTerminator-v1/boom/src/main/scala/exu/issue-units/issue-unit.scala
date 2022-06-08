//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{Str}

import boom.common._
import boom.exu.FUConstants._
import boom.util.{BoolToChar}

/**
 * Class used for configurations        用于配置的类
 *
 * @param issueWidth amount of things that can be issued             可以发行的东西数量
 * @param numEntries size of issue queue                             发行队列的大小
 * @param iqType type of issue queue                                 发行队列的类型
 */
case class IssueParams(
  dispatchWidth: Int = 1,
  issueWidth: Int = 1,
  numEntries: Int = 8,
  iqType: BigInt
)

/**
 * Constants for knowing about the status of a MicroOp              用于了解微操作状态的常量
 */
trait IssueUnitConstants
{
  // invalid  : slot holds no valid uop.
  // s_valid_1: slot holds a valid uop.
  // s_valid_2: slot holds a store-like uop that may be broken into two micro-ops.
  // invalid效: 插槽没有有效的uop。
  // s_valid_1: slot保存一个有效的uop。
  // s_valid_2: slot保存一个类似于store的uop，它可以被分成两个微操作。
  val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(3)
}

/**
 * What physical register is broadcasting its wakeup?
 * Is the physical register poisoned (aka, was it woken up by a speculative issue)?
  * 什么物理寄存器正在广播它的唤醒?
  * *物理寄存器中毒了吗(也就是说，它是被一个投机问题唤醒的)?
 *
 * @param pregSz size of physical destination register
 */
class IqWakeup(val pregSz: Int) extends Bundle
{
  val pdst = UInt(width=pregSz.W)
  val poisoned = Bool()
  val debug_inst = UInt(width=64.W)
  val br_mask = Bool()
  val risk_rob_idx = UInt(32.W)
  val risk = Bool()
}

/**
 * IO bundle to interact with the issue unit       与发行单元互动的IO捆绑
 *
 * @param issueWidth amount of operations that can be issued at once    一次可以发出的操作数量
 * @param numWakeupPorts number of wakeup ports for issue unit          发布单元的唤醒端口数量
 */
class IssueUnitIO(
  val numIssueSlots: Int,
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val dispatchWidth: Int)
  (implicit p: Parameters) extends BoomBundle
{
  val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

  val iss_valids       = Output(Vec(issueWidth, Bool()))
  val iss_uops         = Output(Vec(issueWidth, new MicroOp()))
  val wakeup_ports     = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))

  val spec_ld_wakeup   = Flipped(Vec(memWidth, Valid(UInt(width=maxPregSz.W))))

  // tell the issue unit what each execution pipeline has in terms of functional units
  // 根据功能单元，告诉issue单元每个执行管道有哪些内容
  val fu_types         = Input(Vec(issueWidth, Bits(width=FUC_SZ.W)))

  val brupdate         = Input(new BrUpdateInfo())
  val flush_pipeline   = Input(Bool())
  val ld_miss          = Input(Bool())

  val event_empty      = Output(Bool()) // used by HPM events; is the issue unit empty?

  val tsc_reg          = Input(UInt(width=xLen.W))

  val risk_pdst = Output(Vec(numIssueSlots,UInt(32.W)))
  val risk_debug_inst = Output(Vec(numIssueSlots,UInt(32.W)))
  val risk_rob_idx = Output(Vec(numIssueSlots,UInt(32.W)))
  val idle_cycles = Input(UInt(32.W))
  val return_issue = Input(Bool())

  val risk_table = Input(Vec(numIntPhysRegs, Bool()))
  val fp_risk_table = Input(Vec(numFpPhysRegs, Bool()))
  val st_risk_table = Input(Vec(numIntPhysRegs, Bool()))
  val st_fp_risk_table = Input(Vec(numFpPhysRegs, Bool()))

  val risk_table_interference = Input(Vec(numIntPhysRegs, Bool()))
  val fp_risk_table_interference = Input(Vec(numFpPhysRegs, Bool()))
  val st_risk_table_interference = Input(Vec(numIntPhysRegs, Bool()))
  val st_fp_risk_table_interference = Input(Vec(numFpPhysRegs, Bool()))
  
  val fudiv_interference = Input(Bool())
  val fufdiv_interference = Input(Bool())
  val st_fudiv_interference = Input(Bool())
  val st_fufdiv_interference = Input(Bool())
}

/**
 * Abstract top level issue unit   顶层发布单元
 *
 * @param numIssueSlots depth of issue queue                      发布队列深度
 * @param issueWidth amoutn of operations that can be issued at once
 * @param numWakeupPorts number of wakeup ports for issue unit
 * @param iqType type of issue queue (mem, int, fp)               发行队列的类型
 */
abstract class IssueUnit(
  val numIssueSlots: Int,
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val iqType: BigInt,
  val dispatchWidth: Int)
  (implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
{
  val io = IO(new IssueUnitIO(numIssueSlots, issueWidth, numWakeupPorts, dispatchWidth))

  //-------------------------------------------------------------
  // Set up the dispatch uops
  // special case "storing" 2 uops within one issue slot.
  // 设置调度uops
  // 特殊情况下“存储”2个uops在一个发出槽。

  val dis_uops = Array.fill(dispatchWidth) {Wire(new MicroOp())}
  for (w <- 0 until dispatchWidth) {
    dis_uops(w) := io.dis_uops(w).bits
    dis_uops(w).iw_p1_poisoned := false.B
    dis_uops(w).iw_p2_poisoned := false.B
    dis_uops(w).iw_state := s_valid_1

    if (iqType == IQT_MEM.litValue || iqType == IQT_INT.litValue) {
      // For StoreAddrGen for Int, or AMOAddrGen, we go to addr gen state        对于StoreAddrGen Int或AMOAddrGen，我们进入addr gen状态
      when ((io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype === RT_FIX) ||
             io.dis_uops(w).bits.uopc === uopAMO_AG) {
        dis_uops(w).iw_state := s_valid_2
        // For store addr gen for FP, rs2 is the FP register, and we don't wait for that here   对于FP的存储addr gen, rs2是FP寄存器，我们不在这里等待它
      } .elsewhen (io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype =/= RT_FIX) {
        dis_uops(w).lrs2_rtype := RT_X
        dis_uops(w).prs2_busy  := false.B
      }
      dis_uops(w).prs3_busy := false.B
    } else if (iqType == IQT_FP.litValue) {
      // FP "StoreAddrGen" is really storeDataGen, and rs1 is the integer address register            FP的“StoreAddrGen”实际上是storeDataGen，而rs1是整数地址寄存器
      when (io.dis_uops(w).bits.uopc === uopSTA) {
        dis_uops(w).lrs1_rtype := RT_X
        dis_uops(w).prs1_busy  := false.B
      }
    }

    if (iqType != IQT_INT.litValue) {
      assert(!(io.dis_uops(w).bits.ppred_busy && io.dis_uops(w).valid))
      dis_uops(w).ppred_busy := false.B
    }
  }

  //-------------------------------------------------------------
  // Issue Table

  val slots = for (i <- 0 until numIssueSlots) yield { val slot = Module(new IssueSlot(numWakeupPorts)); slot }
  val issue_slots = VecInit(slots.map(_.io))

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).wakeup_ports := io.wakeup_ports
    issue_slots(i).pred_wakeup_port := io.pred_wakeup_port
    issue_slots(i).spec_ld_wakeup := io.spec_ld_wakeup
    issue_slots(i).ldspec_miss := io.ld_miss
    issue_slots(i).brupdate := io.brupdate
    issue_slots(i).kill := io.flush_pipeline
  }
   


  for (i <- 0 until numIssueSlots) {
    issue_slots(i).return_issue := io.return_issue
    issue_slots(i).idle_cycles := io.idle_cycles
    issue_slots(i).risk_table <> io.risk_table
    issue_slots(i).fp_risk_table <> io.fp_risk_table
    issue_slots(i).st_risk_table <> io.st_risk_table
    issue_slots(i).st_fp_risk_table <> io.st_fp_risk_table
  }

  for (i <- 0 until numIssueSlots) {


    io.risk_pdst(i) := issue_slots(i).risk_pdst
    io.risk_debug_inst(i) := issue_slots(i).risk_debug_inst
    io.risk_rob_idx(i) := issue_slots(i).risk_rob_idx
  }

  io.event_empty := !(issue_slots.map(s => s.valid).reduce(_|_))

  val count = PopCount(slots.map(_.io.valid))
  dontTouch(count)

  //-------------------------------------------------------------

  assert (PopCount(issue_slots.map(s => s.grant)) <= issueWidth.U, "[issue] window giving out too many grants.")


  //-------------------------------------------------------------


  def getType: String =
    if (iqType == IQT_INT.litValue) "int"
    else if (iqType == IQT_MEM.litValue) "mem"
    else if (iqType == IQT_FP.litValue) " fp"
    else "unknown"
}
