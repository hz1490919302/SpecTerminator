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
import chisel3.util.{log2Ceil, PopCount}

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.Str

import FUConstants._
import boom.common._

/**
 * Specific type of issue unit
 *
 * @param params issue queue params
 * @param numWakeupPorts number of wakeup ports for the issue queue
 */
class IssueUnitCollapsing(
  params: IssueParams,
  numWakeupPorts: Int)
  (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
{
  //-------------------------------------------------------------
  // Figure out how much to shift entries by

  val maxShift = dispatchWidth
  val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_uops.map(_.valid).map(!_.asBool)
  val shamts_oh = Array.fill(numIssueSlots+dispatchWidth) {Wire(UInt(width=maxShift.W))}
  // track how many to shift up this entry by by counting previous vacant spots
  def SaturatingCounterOH(count_oh:UInt, inc: Bool, max: Int): UInt = {
     val next = Wire(UInt(width=max.W))
     next := count_oh
     when (count_oh === 0.U && inc) {
       next := 1.U
     } .elsewhen (!count_oh(max-1) && inc) {
       next := (count_oh << 1.U)
     }
     next
  }
  shamts_oh(0) := 0.U
  for (i <- 1 until numIssueSlots + dispatchWidth) {
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), maxShift)
  }

  //-------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  val will_be_valid = (0 until numIssueSlots).map(i => issue_slots(i).will_be_valid) ++
                      (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
                                                        !dis_uops(i).exception &&
                                                        !dis_uops(i).is_fence &&
                                                        !dis_uops(i).is_fencei)

  val uops = issue_slots.map(s=>s.out_uop) ++ dis_uops.map(s=>s)
  for (i <- 0 until numIssueSlots) {
    issue_slots(i).in_uop.valid := false.B
    issue_slots(i).in_uop.bits  := uops(i+1)
    for (j <- 1 to maxShift by 1) {
      when (shamts_oh(i+j) === (1 << (j-1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i+j)
        issue_slots(i).in_uop.bits  := uops(i+j)
      }
    }
    issue_slots(i).clear        := shamts_oh(i) =/= 0.U
  }

  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

  val will_be_available = (0 until numIssueSlots).map(i =>
                            (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
  val num_available = PopCount(will_be_available)
  for (w <- 0 until dispatchWidth) {
    io.dis_uops(w).ready := RegNext(num_available > w.U)
  }

  //-------------------------------------------------------------
  // Issue Select Logic

  // set default
  for (w <- 0 until issueWidth) {
    io.iss_valids(w) := false.B
    io.iss_uops(w)   := NullMicroOp
    // unsure if this is overkill
    io.iss_uops(w).prs1 := 0.U
    io.iss_uops(w).prs2 := 0.U
    io.iss_uops(w).prs3 := 0.U
    io.iss_uops(w).lrs1_rtype := RT_X
    io.iss_uops(w).lrs2_rtype := RT_X
  }

  val requests = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth){Bool()}
  for (w <- 0 until issueWidth) {
    port_issued(w) = false.B
  }

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U

  val prs1_risk1 = Mux(issue_slots(i).uop.lrs1_rtype === RT_FIX, io.risk_table(issue_slots(i).uop.prs1), Mux(issue_slots(i).uop.lrs1_rtype === RT_FLT, io.fp_risk_table(issue_slots(i).uop.prs1), false.B) )
  val prs2_risk1 = Mux(issue_slots(i).uop.lrs2_rtype === RT_FIX, io.risk_table(issue_slots(i).uop.prs2), Mux(issue_slots(i).uop.lrs2_rtype === RT_FLT, io.fp_risk_table(issue_slots(i).uop.prs2), false.B) )
  val prs1_risk_st1 = Mux(issue_slots(i).uop.lrs1_rtype === RT_FIX, io.st_risk_table(issue_slots(i).uop.prs1), Mux(issue_slots(i).uop.lrs1_rtype === RT_FLT, io.st_fp_risk_table(issue_slots(i).uop.prs1), false.B) )
  val prs2_risk_st1 = Mux(issue_slots(i).uop.lrs2_rtype === RT_FIX, io.st_risk_table(issue_slots(i).uop.prs2), Mux(issue_slots(i).uop.lrs2_rtype === RT_FLT, io.st_fp_risk_table(issue_slots(i).uop.prs2), false.B) )
  val all_risk1 = (issue_slots(i).uop.is_br || issue_slots(i).uop.is_jalr) && (prs1_risk1 || prs2_risk1 || prs1_risk_st1 || prs2_risk_st1)   
    
  val prs1_risk1_interference = Mux(issue_slots(i).uop.lrs1_rtype === RT_FIX, io.risk_table_interference(issue_slots(i).uop.prs1), Mux(issue_slots(i).uop.lrs1_rtype === RT_FLT, io.fp_risk_table_interference(issue_slots(i).uop.prs1), false.B) )
  val prs2_risk1_interference = Mux(issue_slots(i).uop.lrs2_rtype === RT_FIX, io.risk_table_interference(issue_slots(i).uop.prs2), Mux(issue_slots(i).uop.lrs2_rtype === RT_FLT, io.fp_risk_table_interference(issue_slots(i).uop.prs2), false.B) )
  val prs1_risk_st1_interference = Mux(issue_slots(i).uop.lrs1_rtype === RT_FIX, io.st_risk_table_interference(issue_slots(i).uop.prs1), Mux(issue_slots(i).uop.lrs1_rtype === RT_FLT, io.st_fp_risk_table_interference(issue_slots(i).uop.prs1), false.B) )
  val prs2_risk_st1_interference = Mux(issue_slots(i).uop.lrs2_rtype === RT_FIX, io.st_risk_table_interference(issue_slots(i).uop.prs2), Mux(issue_slots(i).uop.lrs2_rtype === RT_FLT, io.st_fp_risk_table_interference(issue_slots(i).uop.prs2), false.B) )
  val all_risk1_interference = ( issue_slots(i).uop.fu_code === FU_FDV && (io.fufdiv_interference === true.B) && ((prs1_risk1_interference === true.B) || (prs2_risk1_interference === true.B)) ) ||
                               ( issue_slots(i).uop.fu_code === FU_FDV && (io.st_fufdiv_interference === true.B) && ((prs1_risk_st1_interference === true.B) || (prs2_risk_st1_interference === true.B)) )

  val all_risk2_interference = ( issue_slots(i).uop.fu_code === FU_DIV && (io.fudiv_interference === true.B) && ((prs1_risk1_interference === true.B) || (prs2_risk1_interference === true.B)) ) ||
                               ( issue_slots(i).uop.fu_code === FU_DIV && (io.st_fudiv_interference === true.B) && ((prs1_risk_st1_interference === true.B) || (prs2_risk_st1_interference === true.B)) )   
      when (requests(i) && !uop_issued && can_allocate && !port_issued(w) && !all_risk1 && true.B ) { //&& !(all_risk1_interference || all_risk2_interference)
        issue_slots(i).grant := true.B
        io.iss_valids(w) := true.B
        io.iss_uops(w) := issue_slots(i).uop
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }
}
