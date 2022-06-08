//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename BusyTable
// Busy Table中维护着每个物理寄存器的"Busy"状态（即该寄存器当前是否可读），当某个寄存器被分配作为pdst时，BusyTable的对应表项会置位，当该指令写回时，BusyTable对应表项清空。
// 当且仅当某条指令所有操作数均为清空状态时，该指令才能发射。
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.exu

import chisel3.{RegInit, printf, _}
import chisel3.util._
import boom.common._
import boom.util._
import freechips.rocketchip.config.Parameters

class BusyResp extends Bundle
{
  val prs1_busy = Bool()
  val prs2_busy = Bool()
  val prs3_busy = Bool()
}

/*class RiskResp extends Bundle
{
  val risk = Bool()
}*/

//跟踪物理寄存器的准备状态。若准备就绪，则指令准备发出
class RenameBusyTable(
  val plWidth: Int,
  val numPregs: Int,
  val numWbPorts: Int,
  val bypass: Boolean,
  val float: Boolean)
  (implicit p: Parameters) extends BoomModule
{
  val pregSz = log2Ceil(numPregs)

  val io = IO(new BoomBundle()(p) {
    val ren_uops = Input(Vec(plWidth, new MicroOp))
    val busy_resps = Output(Vec(plWidth, new BusyResp))
    //val risk_resps = Output(Vec(plWidth, new RiskResp))
    val rebusy_reqs = Input(Vec(plWidth, Bool()))

    val wb_pdsts = Input(Vec(numWbPorts, UInt(pregSz.W)))   //写回物理目的寄存器
    val wb_valids = Input(Vec(numWbPorts, Bool()))   //写回有效性

    val debug = new Bundle { val busytable = Output(Bits(numPregs.W)) }

    //val clear_risk_table = Input(UInt(pregSz.W))
  })

  val busy_table = RegInit(0.U(numPregs.W))
  //val risk_table = RegInit(VecInit(Seq.fill(numPregs){false.B}))
  // Unbusy written back registers.   不繁忙的回写寄存器。
  val busy_table_wb = busy_table & ~(io.wb_pdsts zip io.wb_valids)
    .map {case (pdst, valid) => UIntToOH(pdst) & Fill(numPregs, valid.asUInt)}.reduce(_|_)
  // Rebusy newly allocated registers.  重新处理新分配的寄存器。
  val busy_table_next = busy_table_wb | (io.ren_uops zip io.rebusy_reqs)
    .map {case (uop, req) => UIntToOH(uop.pdst) & Fill(numPregs, req.asUInt)}.reduce(_|_)

  busy_table := busy_table_next


  /*for (i <- 0 until plWidth) {
      when(io.ren_uops(i).br_mask =/= 0.U && io.ren_uops(i).uses_ldq && risk_table(io.ren_uops(i).prs1) === 0.B && risk_table(io.ren_uops(i).prs2) === 0.B && risk_table(io.ren_uops(i).prs3) === 0.B){
        risk_table(io.ren_uops(i).pdst) := true.B
        io.risk_resps(i).risk := true.B
       // printf("rename-load 0x%x \n", io.ren_uops(i).debug_inst)
        when(io.ren_uops(i).debug_inst === 0x00054703L.U){
          printf(p"0x00054703L:io.ren_uops(i).pdst=${io.ren_uops(i).pdst} ")
          printf(p"risk_table=${risk_table} \n")
        }
      }
      .elsewhen((risk_table(io.ren_uops(i).prs1)===true.B || risk_table(io.ren_uops(i).prs2)===true.B || risk_table(io.ren_uops(i).prs3)===true.B) && !io.ren_uops(i).uses_ldq){
        risk_table(io.ren_uops(i).pdst) := true.B
        io.risk_resps(i).risk := true.B
        //printf("rename-not-load 0x%x \n", io.ren_uops(i).debug_inst)
        when(io.ren_uops(i).debug_inst(15,0) === 0x97baL.U){
          printf(p"0x97baL:io.ren_uops(i).pdst=${io.ren_uops(i).pdst} ")
          printf(p"risk_table=${risk_table} \n")
        }
      }
        .otherwise{
          io.risk_resps(i).risk := false.B
        }

    when(io.ren_uops(i).debug_inst === 0x9a878793L.U){
      printf(p"0x9a878793L:io.ren_uops(i).pdst=${io.ren_uops(i).pdst} ")
      printf(p"io.clear_risk_table=${io.clear_risk_table} ")
      printf(p"risk_table1=${risk_table} \n")
    }

    when(io.ren_uops(i).debug_inst(15,0) === 0x97baL.U){
      printf(p"0x97baL:io.ren_uops(i).prs2=${io.ren_uops(i).prs2} ")
      printf(p"prs2_risk_table=${risk_table(io.ren_uops(i).prs2)} ")
      printf(p"risk_table2=${risk_table} \n")
    }

  }

  //UIntToOH(0)=1
  when(io.clear_risk_table =/= 0.U) {
    risk_table(io.clear_risk_table) :=  false.B
  }*/


  // Read the busy table.  阅读忙表。
  for (i <- 0 until plWidth) {
    val prs1_was_bypassed = (0 until i).map(j =>
      io.ren_uops(i).lrs1 === io.ren_uops(j).ldst && io.rebusy_reqs(j)).foldLeft(false.B)(_||_)
    val prs2_was_bypassed = (0 until i).map(j =>
      io.ren_uops(i).lrs2 === io.ren_uops(j).ldst && io.rebusy_reqs(j)).foldLeft(false.B)(_||_)
    val prs3_was_bypassed = (0 until i).map(j =>
      io.ren_uops(i).lrs3 === io.ren_uops(j).ldst && io.rebusy_reqs(j)).foldLeft(false.B)(_||_)

    io.busy_resps(i).prs1_busy := busy_table(io.ren_uops(i).prs1) || prs1_was_bypassed && bypass.B
    io.busy_resps(i).prs2_busy := busy_table(io.ren_uops(i).prs2) || prs2_was_bypassed && bypass.B
    io.busy_resps(i).prs3_busy := busy_table(io.ren_uops(i).prs3) || prs3_was_bypassed && bypass.B
    if (!float) io.busy_resps(i).prs3_busy := false.B
  }

  io.debug.busytable := busy_table
}
