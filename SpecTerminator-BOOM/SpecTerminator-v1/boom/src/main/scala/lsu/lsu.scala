//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Out-of-Order Load/Store Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Load/Store Unit is made up of the Load-Address Queue, the Store-Address
// Queue, and the Store-Data queue (LAQ, SAQ, and SDQ).
//
// Stores are sent to memory at (well, after) commit, loads are executed
// optimstically ASAP.  If a misspeculation was discovered, the pipeline is
// cleared. Loads put to sleep are retried.  If a LoadAddr and StoreAddr match,
// the Load can receive its data by forwarding data out of the Store-Data
// Queue.
//
// Currently, loads are sent to memory immediately, and in parallel do an
// associative search of the SAQ, on entering the LSU. If a hit on the SAQ
// search, the memory request is killed on the next cycle, and if the SDQ entry
// is valid, the store data is forwarded to the load (delayed to match the
// load-use delay to delay with the write-port structural hazard). If the store
// data is not present, or it's only a partial match (SB->LH), the load is put
// to sleep in the LAQ.
//
// Memory ordering violations are detected by stores at their addr-gen time by
// associatively searching the LAQ for newer loads that have been issued to
// memory.
//
// The store queue contains both speculated and committed stores.
//
// Only one port to memory... loads and stores have to fight for it, West Side
// Story style.
//
// TODO:
//    - Add predicting structure for ordering failures
//    - currently won't STD forward if DMEM is busy
//    - ability to turn off things if VM is disabled
//    - reconsider port count of the wakeup, retry stuff

package boom.lsu

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Str

import boom.common._
import boom.exu.{BrUpdateInfo, Exception, FuncUnitResp, CommitSignals, ExeUnitResp}
import boom.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask}

/***
* 加载/存储单元（LSU）负责决定何时启动内存操作到内存系统。
* 有两个队列：加载队列（LDQ）和存储队列（STQ）。加载指令生成一个“ uopLD”微操作（UOP）。
* 发出后，“ uopLD”将计算加载地址并将其结果放置在LDQ中。存储指令（可以）生成两个 UOP，即“ uopSTA”（存储地址生成）和“ uopSTD”（存储数据生成）。
* STA UOP计算存储地址，并将其结果放入SAQ队列中。STD UOP将存储数据从寄存器文件移至SDQ。
* 这些UOP中的每一个都会在 操作数准备就绪后，立即发出窗口。
***/

class LSUExeIO(implicit p: Parameters) extends BoomBundle()(p)
{
  // The "resp" of the maddrcalc is really a "req" to the LSU       maddrcalc的“resp”实际上是对LSU的“请求”
  val req       = Flipped(new ValidIO(new FuncUnitResp(xLen)))
  // Send load data to regfiles                                     发送加载数据到regfiles
  val iresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen))
  val fresp    = new DecoupledIO(new boom.exu.ExeUnitResp(xLen+1)) // TODO: Should this be fLen?
}

class BoomDCacheReq(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
  val addr  = UInt(coreMaxAddrBits.W)
  val data  = Bits(coreDataBits.W)
  val is_hella = Bool() // Is this the hellacache req? If so this is not tracked in LDQ or STQ  这是hellacache的要求吗?如果是这样，则不会在LDQ或STQ中跟踪
}

class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
  val data = Bits(coreDataBits.W)
  val is_hella = Bool()
}

class LSUDMemIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
{
  val risk_table = Output(Vec(numIntPhysRegs,Bool()))
  val fp_risk_table = Output(Vec(numFpPhysRegs,Bool()))
  val st_risk_table = Output(Vec(numIntPhysRegs,Bool()))
  val st_fp_risk_table = Output(Vec(numFpPhysRegs,Bool()))

  // In LSU's dmem stage, send the request                                         //在LSU的dmem阶段，发送请求
  val req         = new DecoupledIO(Vec(memWidth, Valid(new BoomDCacheReq)))
  // In LSU's LCAM search stage, kill if order fail (or forwarding possible)    //在LSU的LCAM搜索阶段，如果排序失败(或可能转发)，杀死
  val s1_kill     = Output(Vec(memWidth, Bool()))
  // Get a request any cycle                                                 //获得一个请求任何周期
  val resp        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheResp)))
  // In our response stage, if we get a nack, we need to reexecute         //在我们的响应阶段，如果我们得到一个nack，我们需要重新执行
  val nack        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheReq)))

  val brupdate       = Output(new BrUpdateInfo)
  val exception    = Output(Bool())
  val rob_pnr_idx  = Output(UInt(robAddrSz.W))
  val rob_head_idx = Output(UInt(robAddrSz.W))

  val release = Flipped(new DecoupledIO(new TLBundleC(edge.bundle)))

  // Clears prefetching MSHRs            清除预取MSHRs
  val force_order  = Output(Bool())
  val ordered     = Input(Bool())

  val idle_cycles = Output(UInt(32.W))

  val perf = Input(new Bundle {
    val acquire = Bool()
    val release = Bool()
  })

  val missnum = Input(UInt(32.W))

  override def cloneType = new LSUDMemIO().asInstanceOf[this.type]
}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
{
  val exe = Vec(memWidth, new LSUExeIO)

  val risk_table = Input(Vec(numIntPhysRegs,Bool()))
  val fp_risk_table = Input(Vec(numFpPhysRegs,Bool()))
  val st_risk_table = Input(Vec(numIntPhysRegs,Bool()))
  val st_fp_risk_table = Input(Vec(numFpPhysRegs,Bool()))

  val idle_cycles = Input(UInt(32.W))
  val storeaddr_not_get = Output(Bool())
  val store_wuran = Output(Bool())
  val ldq_dep_mask = Output(Vec(numLdqEntries,UInt(numLdqEntries.W)))
  val ldq_valid = Output(Vec(numLdqEntries,Bool()))
  val stq_valid = Output(Vec(numStqEntries,Bool()))
  val stq_addr_valid = Output(Vec(numStqEntries,Bool()))
  val stq_addr_virtual =  Output(Vec(numStqEntries,Bool()))
  

  val dis_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))    //分派微操作
  val dis_ldq_idx = Output(Vec(coreWidth, UInt(ldqAddrSz.W)))      //分派—ldq-索引
  val dis_stq_idx = Output(Vec(coreWidth, UInt(stqAddrSz.W)))      //分派-stq-索引

  val ldq_full    = Output(Vec(coreWidth, Bool()))                 //ldq满
  val stq_full    = Output(Vec(coreWidth, Bool()))                 //stq满

  val fp_stdata   = Flipped(Decoupled(new ExeUnitResp(fLen)))

  val commit      = Input(new CommitSignals)                       //提交
  val commit_load_at_rob_head = Input(Bool())                      //在rob head提交的是load

  // Stores clear busy bit when stdata is received
  // memWidth for int, 1 for fp (to avoid back-pressure fpstdat)
  // store在接收到stdata时清除繁忙位
  // memWidth为int, 1为fp(以避免反压fpstdat)
  val clr_bsy         = Output(Vec(memWidth + 1, Valid(UInt(robAddrSz.W))))

  // Speculatively safe load (barring memory ordering failure)          推测的安全负载(阻止内存排序失败)
  val clr_unsafe      = Output(Vec(memWidth, Valid(UInt(robAddrSz.W))))

  // Tell the DCache to clear prefetches/speculating misses             告诉DCache清除预取/猜测失败
  val fence_dmem   = Input(Bool())

  // Speculatively tell the IQs that we'll get load data back next cycle    //推测地告诉IQs(issue queue?)，我们将在下一个周期得到加载数据返回
  val spec_ld_wakeup = Output(Vec(memWidth, Valid(UInt(maxPregSz.W))))
  // Tell the IQs that the load we speculated last cycle was misspeculated  //告诉IQs，我们对上个周期的load预测是错误的
  val ld_miss      = Output(Bool())

  val brupdate       = Input(new BrUpdateInfo)
  val rob_pnr_idx  = Input(UInt(robAddrSz.W))
  val rob_head_idx = Input(UInt(robAddrSz.W))
  val exception    = Input(Bool())

  val fencei_rdy  = Output(Bool())

  val lxcpt       = Output(Valid(new Exception))

  val tsc_reg     = Input(UInt())

  val perf        = Output(new Bundle {
    val acquire = Bool()
    val release = Bool()
    val tlbMiss = Bool()
  })

  val missnum = Output(UInt(32.W))
  val misstlbnum = Output(UInt(32.W))
}

class LSUIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
{
  val ptw   = new rocket.TLBPTWIO
  val core  = new LSUCoreIO
  val dmem  = new LSUDMemIO

  val hellacache = Flipped(new freechips.rocketchip.rocket.HellaCacheIO)
}

class LDQEntry(implicit p: Parameters) extends BoomBundle()(p)
    with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss               //虚拟地址，我们有一个TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute   //不可缓存的，等到rob head执行

  val executed            = Bool() // load sent to memory, reset by NACKs              //加载发送到内存，由NACKs重置
  val succeeded           = Bool()
  val order_fail          = Bool()
  val observed            = Bool()
  //val store_risk          = Bool()

  val preorder_fail = UInt(4.W)

  val st_dep_mask         = UInt(numStqEntries.W) // list of stores older than us         //比我们更老的store名单
  val youngest_stq_idx    = UInt(stqAddrSz.W) // index of the oldest store younger than us  //比我们更年轻的 最老的store索引

  val forward_std_val     = Bool()
  val forward_stq_idx     = UInt(stqAddrSz.W) // Which store did we get the store-load forward from?    //我们从哪个store中获得存储加载转发?

  val debug_wb_data       = UInt(xLen.W)
}

class STQEntry(implicit p: Parameters) extends BoomBundle()(p)
   with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss              //虚拟地址，我们有一个TLB miss
  val data                = Valid(UInt(xLen.W))

  val preorder_fail = UInt(4.W)

  val committed           = Bool() // committed by ROB                                //被rob提交
  val succeeded           = Bool() // D$ has ack'd this, we don't need to maintain this anymore  Dcache已经应答了这个，我们不需要再维持它了

  val debug_wb_data       = UInt(xLen.W)
}

class LSU(implicit p: Parameters, edge: TLEdgeOut) extends BoomModule()(p)
  with rocket.HasL1HellaCacheParameters
{
  val io = IO(new LSUIO)


  val ldq = Reg(Vec(numLdqEntries, Valid(new LDQEntry)))          //创建ldq
  val stq = Reg(Vec(numStqEntries, Valid(new STQEntry)))          //创建stq

  val xx = RegInit(VecInit(Seq.fill(numStqEntries){true.B}))
  val yy = RegInit(VecInit(Seq.fill(numStqEntries){true.B}))


  val ldq_head         = Reg(UInt(ldqAddrSz.W))
  val ldq_tail         = Reg(UInt(ldqAddrSz.W))
  val stq_head         = Reg(UInt(stqAddrSz.W)) // point to next store to clear from STQ (i.e., send to memory)  指向下一个要从STQ中清除的store(即，发送到内存)
  val stq_tail         = Reg(UInt(stqAddrSz.W))
  val stq_commit_head  = Reg(UInt(stqAddrSz.W)) // point to next store to commit      指向下一个要提交的store
  val stq_execute_head = Reg(UInt(stqAddrSz.W)) // point to next store to execute     指向下一个要执行的store


  // If we got a mispredict, the tail will be misaligned for 1 extra cycle    //如果我们的预测错误，尾部将会多排列一个周期
  assert (io.core.brupdate.b2.mispredict ||
          stq(stq_execute_head).valid ||
          stq_head === stq_execute_head ||
          stq_tail === stq_execute_head,
            "stq_execute_head got off track.")
  //stq_execute_head脱离了轨道。

  //dcache创建
  val h_ready :: h_s1 :: h_s2 :: h_s2_nack :: h_wait :: h_replay :: h_dead :: Nil = Enum(7)
  // s1 : do TLB, if success and not killed, fire request go to h_s2
  //      store s1_data to register
  //      if tlb miss, go to s2_nack
  //      if don't get TLB, go to s2_nack
  //      store tlb xcpt
  // s2 : If kill, go to dead
  //      If tlb xcpt, send tlb xcpt, go to dead
  // s2_nack : send nack, go to dead
  // wait : wait for response, if nack, go to replay
  // replay : refire request, use already translated address
  // dead : wait for response, ignore it

  // s1:执行TLB，如果成功且未终止，则触发请求转到h_s2，存储s1_data到寄存器
  //      如果tlb未命中，转到s2_nack
  //      如果没有获得TLB，转到s2_nack， store tlb xcpt
  // s2:如果kill，就去dead
  //      如果tlb xcpt，发送tlb xcpt，去dead
  // s2_nack:发送nack，去dead
  // wait:等待响应，如果nack，去replay
  // replay：重发请求，使用已翻译的地址
  // dead:等待响应，忽略它
  val hella_state           = RegInit(h_ready)
  val hella_req             = Reg(new rocket.HellaCacheReq)
  val hella_data            = Reg(new rocket.HellaCacheWriteData)
  val hella_paddr           = Reg(UInt(paddrBits.W))
  val hella_xcpt            = Reg(new rocket.HellaCacheExceptions)

  //tlb创建
  val dtlb = Module(new NBDTLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBSets, dcacheParams.nTLBWays)))

  io.ptw <> dtlb.io.ptw                    //io.ptw；连接dtlb的页表遍历
  io.core.misstlbnum := dtlb.io.misstlbnum
  io.core.perf.tlbMiss := io.ptw.req.fire()
  io.core.perf.acquire := io.dmem.perf.acquire
  io.core.perf.release := io.dmem.perf.release

  io.core.missnum := io.dmem.missnum

  val clear_store     = WireInit(false.B)
  val live_store_mask = RegInit(0.U(numStqEntries.W))
  var next_live_store_mask = Mux(clear_store, live_store_mask & ~(1.U << stq_head),
                                              live_store_mask)


  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Enqueue new entries            入队新条目
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // This is a newer store than existing loads, so clear the bit in all the store dependency masks
  //这是一个比现有load更新的store，因此清除所有store依赖掩码中的位   （感觉是要从stq head中清除一个store，将所有ldq中的条目的st_dep_mask更新）
  for (i <- 0 until numLdqEntries)
  {
    when (clear_store)
    {
      ldq(i).bits.st_dep_mask := ldq(i).bits.st_dep_mask & ~(1.U << stq_head)          //st_dep_mask：比我们更老的store名单
    } //stq_head:指向下一个要从STQ中清除的store(即，发送到内存)
  }

  // Decode stage
  var ld_enq_idx = ldq_tail
  var st_enq_idx = stq_tail

  val stq_nonempty = (0 until numStqEntries).map{ i => stq(i).valid }.reduce(_||_) =/= 0.U

  var ldq_full = Bool()
  var stq_full = Bool()

  for (w <- 0 until coreWidth)
  {
    ldq_full = WrapInc(ld_enq_idx, numLdqEntries) === ldq_head              //从ldq_tail入队，ld_enq_idx增加后==ldq_head，则ldq为满
    io.core.ldq_full(w)    := ldq_full
    io.core.dis_ldq_idx(w) := ld_enq_idx

    stq_full = WrapInc(st_enq_idx, numStqEntries) === stq_head              //从stq_tail入队，st_enq_idx增加后==stq_head，则stq为满
    io.core.stq_full(w)    := stq_full
    io.core.dis_stq_idx(w) := st_enq_idx

    val dis_ld_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception   //若dis_uops使用uses_ldq
    val dis_st_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception   //若dis_uops使用uses_stq
    when (dis_ld_val)   //load,更新ldq条目
    {
      ldq(ld_enq_idx).valid                := true.B
      ldq(ld_enq_idx).bits.uop             := io.core.dis_uops(w).bits     //dis_uops赋给ldq.bits.uop
      ldq(ld_enq_idx).bits.youngest_stq_idx  := st_enq_idx          //比我们更年轻的 最老的store索引
      ldq(ld_enq_idx).bits.st_dep_mask     := next_live_store_mask    //比我们更老的store名单

      ldq(ld_enq_idx).bits.addr.valid      := false.B
      ldq(ld_enq_idx).bits.executed        := false.B
      ldq(ld_enq_idx).bits.succeeded       := false.B
      ldq(ld_enq_idx).bits.order_fail      := false.B
      ldq(ld_enq_idx).bits.observed        := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B
      ldq(ld_enq_idx).bits.preorder_fail   := 9.U
      //ldq(ld_enq_idx).bits.store_risk      := (0 until numStqEntries).map{ k => ((1.U << k) & next_live_store_mask) =/= 0.U && stq(k).valid && (io.core.st_risk_table(stq(k).bits.uop.prs1) || io.core.risk_table(stq(k).bits.uop.prs1)) && stq(k).bits.uop.lrs1_rtype === RT_FIX  }.reduce(_||_)

      //不匹配的入队load标签
      assert (ld_enq_idx === io.core.dis_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
      //入队uop正在覆盖ldq条目
      assert (!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")


    }
      .elsewhen (dis_st_val)   //store，更新stq条目
    {
      stq(st_enq_idx).valid           := true.B
      stq(st_enq_idx).bits.uop        := io.core.dis_uops(w).bits
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.committed  := false.B
      stq(st_enq_idx).bits.succeeded  := false.B
      stq(st_enq_idx).bits.preorder_fail := 9.U       
 
      xx(st_enq_idx) := true.B
      yy(st_enq_idx) := true.B

      //不匹配的入队store标签
      assert (st_enq_idx === io.core.dis_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
      //入队uop正在覆盖stq条目
      assert (!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
    }

    ld_enq_idx = Mux(dis_ld_val, WrapInc(ld_enq_idx, numLdqEntries),
                                 ld_enq_idx)           //若dis中来的是load，ld_enq_idx递增

    next_live_store_mask = Mux(dis_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)          //更新next_live_store_mask，若是store，掩码或上新入队的store
    st_enq_idx = Mux(dis_st_val, WrapInc(st_enq_idx, numStqEntries),
                                 st_enq_idx)           //若dis中来的是store，st_enq_idx递增

    //一个UOP试图进入LDQ和STQ
    assert(!(dis_ld_val && dis_st_val), "A UOP is trying to go into both the LDQ and the STQ")
  }

  ldq_tail := ld_enq_idx         //更新ldq_tail
  stq_tail := st_enq_idx         //更新stq_tail

  //以下dmem目前不清楚是啥意思
  io.dmem.force_order   := io.core.fence_dmem
  io.core.fencei_rdy    := !stq_nonempty && io.dmem.ordered

  io.dmem.risk_table <> io.core.risk_table
  io.dmem.fp_risk_table <> io.core.fp_risk_table
  io.dmem.st_risk_table <> io.core.st_risk_table
  io.dmem.st_fp_risk_table <> io.core.st_fp_risk_table

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Execute stage (access TLB, send requests to Memory)           执行阶段(访问TLB，发送请求到内存)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // We can only report 1 exception per cycle.
  // Just be sure to report the youngest one   //每个周期只能报告一个异常。一定要报最小的那个
  val mem_xcpt_valid  = Wire(Bool())
  val mem_xcpt_cause  = Wire(UInt())
  val mem_xcpt_uop    = Wire(new MicroOp)
  val mem_xcpt_vaddr  = Wire(UInt())


  //---------------------------------------
  // Can-fire logic and wakeup/retry select
  //
  // First we determine what operations are waiting to execute.
  // These are the "can_fire"/"will_fire" signals
  //可以发出逻辑和唤醒/重试选择
  //
  //首先确定哪些操作等待执行。
  //这些是"can_fire"/"will_fire"信号

  val will_fire_load_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_stad_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_std_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_sfence         = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_incoming = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_wakeup   = Wire(Vec(memWidth, Bool()))
  val will_fire_release        = Wire(Vec(memWidth, Bool()))
  val will_fire_load_retry     = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_retry      = Wire(Vec(memWidth, Bool()))
  val will_fire_store_commit   = Wire(Vec(memWidth, Bool()))
  val will_fire_load_wakeup    = Wire(Vec(memWidth, Bool()))

  val exe_req = WireInit(VecInit(io.core.exe.map(_.req)))
  // Sfence goes through all pipes           //Sfence通过所有管道   //应该是sfence序列化指令的逻辑
  for (i <- 0 until memWidth) {
    when (io.core.exe(i).req.bits.sfence.valid) {       //若sfence有效，给exe_req赋值
      exe_req := VecInit(Seq.fill(memWidth) { io.core.exe(i).req })
    }
  }

  // -------------------------------
  // Assorted signals for scheduling                   //组合调度信号

  // Don't wakeup a load if we just sent it last cycle or two cycles ago
  // The block_load_mask may be wrong, but the executing_load mask must be accurate
  //不要唤醒一个负载，如果我们刚刚发送它在上一个周期或两个周期之前
  //block_load_mask可能是错误的，但是executing_load mask必须是准确的
  val block_load_mask    = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))
  val p1_block_load_mask = RegNext(block_load_mask)
  val p2_block_load_mask = RegNext(p1_block_load_mask)

 // Prioritize emptying the store queue when it is almost full    //当store队列几乎满时，优先清空它
  val stq_almost_full = RegNext(WrapInc(WrapInc(st_enq_idx, numStqEntries), numStqEntries) === stq_head ||
                                WrapInc(st_enq_idx, numStqEntries) === stq_head)

  // The store at the commit head needs the DCache to appear ordered
  // Delay firing load wakeups and retries now
  // commit head的store需要DCache显示有序
  // 现在延迟发出 load唤醒和重试
  val store_needs_order = WireInit(false.B)

  val ldq_incoming_idx = widthMap(i => exe_req(i).bits.uop.ldq_idx)
  val ldq_incoming_e   = widthMap(i => ldq(ldq_incoming_idx(i)))

  val stq_incoming_idx = widthMap(i => exe_req(i).bits.uop.stq_idx)
  val stq_incoming_e   = widthMap(i => stq(stq_incoming_idx(i)))
  
   
   for(i <- 0 until 8){
        when(stq(i).valid && io.core.st_risk_table(stq(i).bits.uop.prs1) === false.B){
            xx(i) := false.B
        }
        when(stq(i).valid && io.core.risk_table(stq(i).bits.uop.prs1) === false.B){
            yy(i) := false.B
        }
    }
    
    

  val ldq_retry_idx = RegNext(AgePriorityEncoder((0 until numLdqEntries).map(i => {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    val store_rid_risk = (0 until numStqEntries).map{ k =>((1.U << k) & ldq(i).bits.st_dep_mask) =/= 0.U && stq(k).valid && (xx(k) || yy (k)) && stq(k).bits.uop.lrs1_rtype === RT_FIX  }.reduce(_||_)
    e.addr.valid && e.addr_is_virtual && !block && !store_rid_risk
  }), ldq_head))                                              //ldq重试idx，其中AgePriorityEncoder的逻辑是？
  val ldq_retry_e            = ldq(ldq_retry_idx)             //ldq重试条目

  val stq_retry_idx = RegNext(AgePriorityEncoder((0 until numStqEntries).map(i => {
    val e = stq(i).bits
    e.addr.valid && e.addr_is_virtual
  }), stq_commit_head))                                       //stq重试idx
  val stq_retry_e   = stq(stq_retry_idx)                      //stq重试条目

  val stq_commit_e  = stq(stq_execute_head)                   //stq_execute_head:指向下一个要执行的store

  val ldq_wakeup_idx = RegNext(AgePriorityEncoder((0 until numLdqEntries).map(i=> {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && !e.executed && !e.succeeded && !e.addr_is_virtual && !block
  }), ldq_head))                                              //ldq唤醒idx，唤醒与重试的区别？
  val ldq_wakeup_e   = ldq(ldq_wakeup_idx)                    //ldq唤醒条目

  // -----------------------
  // Determine what can fire           确定什么可以发射
  

  // Can we fire a incoming load                                我们能发射传入的load吗
  val can_fire_load_incoming = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_load)// && (exe_req(w).bits.uop.riskstore_after_load === false.B))
  
  /*when(exe_req(0).valid && exe_req(0).bits.uop.ctrl.is_load && (exe_req(0).bits.uop.riskstore_after_load =/= false.B)){
      //ldq(exe_req(0).bits.uop.ldq_idx).bits.store_risk := true.B
      printf(p"can fire load incoming")
      printf("pc=0x%x\n",exe_req(0).bits.uop.debug_pc)
      printf("ldq_idx=%d\n",exe_req(0).bits.uop.ldq_idx)
      printf("ldq valid=%d\n",ldq(exe_req(0).bits.uop.ldq_idx).valid)
  }
  for(i <- 0 until numLdqEntries){
      when(ldq(i).valid && ldq(i).bits.uop.riskstore_after_load === true.B && not_store_wuran){
          ldq(i).bits.uop.riskstore_after_load === false.B
          can_fire_load_reincoming
      }
  }*/

  // Can we fire an incoming store addrgen + store datagen      我们可以发送一个传入的store addrgen + store datagen吗
  val can_fire_stad_incoming = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_sta
                                                              && exe_req(w).bits.uop.ctrl.is_std)

  // Can we fire an incoming store addrgen                      我们可以发送一个传入的store addrgen吗
  val can_fire_sta_incoming  = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_sta
                                                              && !exe_req(w).bits.uop.ctrl.is_std)

  // Can we fire an incoming store datagen                      我们可以发送一个传入的store datagen吗
  val can_fire_std_incoming  = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_std
                                                              && !exe_req(w).bits.uop.ctrl.is_sta)

  // Can we fire an incoming sfence                             我们可以发送一个传入的sfence吗
  val can_fire_sfence        = widthMap(w => exe_req(w).valid && exe_req(w).bits.sfence.valid)

  // Can we fire a request from dcache to release a line        我们能发出请求从dcache释放一行吗
  // This needs to go through LDQ search to mark loads as dangerous   //这需要通过LDQ搜索来将load标记为危险
  val can_fire_release       = widthMap(w => (w == memWidth-1).B && io.dmem.release.valid)
  io.dmem.release.ready     := will_fire_release.reduce(_||_)

  // Can we retry a load that missed in the TLB                 我们能重试TLB中没有命中的load吗
  val can_fire_load_retry    = widthMap(w =>
                               ( ldq_retry_e.valid                            &&
                                 ldq_retry_e.bits.addr.valid                  &&
                                 ldq_retry_e.bits.addr_is_virtual             &&
                                !p1_block_load_mask(ldq_retry_idx)            &&
                                !p2_block_load_mask(ldq_retry_idx)            &&
                                RegNext(dtlb.io.miss_rdy)                     &&
                                !store_needs_order                            &&
                                (w == memWidth-1).B                           && // TODO: Is this best scheduling? TODO：这是最好的计划吗?
                                !ldq_retry_e.bits.order_fail))

  // Can we retry a store addrgen that missed in the TLB        我们可以重试TLB中命中丢失的addrgen store吗
  // - Weird edge case when sta_retry and std_incoming for same entry in same cycle. Delay this
  // -奇怪的边缘情况，当sta_retry和std_incoming相同的条目在同一个周期。推迟
  val can_fire_sta_retry     = widthMap(w =>
                               ( stq_retry_e.valid                            &&
                                 stq_retry_e.bits.addr.valid                  &&
                                 stq_retry_e.bits.addr_is_virtual             &&
                                 (w == memWidth-1).B                          &&
                                 RegNext(dtlb.io.miss_rdy)                    &&
                                 !(widthMap(i => (i != w).B               &&
                                                 can_fire_std_incoming(i) &&
                                                 stq_incoming_idx(i) === stq_retry_idx).reduce(_||_))
                               ))
  // Can we commit a store                                      我们能commit store吗
  val can_fire_store_commit  = widthMap(w =>
                               ( stq_commit_e.valid                           &&
                                !stq_commit_e.bits.uop.is_fence               &&
                                !mem_xcpt_valid                               &&
                                !stq_commit_e.bits.uop.exception              &&
                                (w == 0).B                                    &&
                                (stq_commit_e.bits.committed || ( stq_commit_e.bits.uop.is_amo      &&
                                                                  stq_commit_e.bits.addr.valid      &&
                                                                 !stq_commit_e.bits.addr_is_virtual &&
                                                                  stq_commit_e.bits.data.valid))))   //amo指令是？

  // Can we wakeup a load that was nack'd                      我们可以唤醒一个load that是nack的吗
  // load在commit head && 唤醒idx==ldq head && 该load前没有更老的store
  val block_load_wakeup = WireInit(false.B)
  val can_fire_load_wakeup = widthMap(w =>
                             ( ldq_wakeup_e.valid                                      &&
                               ldq_wakeup_e.bits.addr.valid                            &&
                              !ldq_wakeup_e.bits.succeeded                             &&
                              !ldq_wakeup_e.bits.addr_is_virtual                       &&
                              !ldq_wakeup_e.bits.executed                              &&
                              !ldq_wakeup_e.bits.order_fail                            &&
                              !p1_block_load_mask(ldq_wakeup_idx)                      &&
                              !p2_block_load_mask(ldq_wakeup_idx)                      &&
                              !store_needs_order                                       &&
                              !block_load_wakeup                                       &&
                              (w == memWidth-1).B                                      &&
                              (!ldq_wakeup_e.bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head &&
                                                                          ldq_head === ldq_wakeup_idx &&
                                                                          ldq_wakeup_e.bits.st_dep_mask.asUInt === 0.U))))


  // Can we fire an incoming hellacache request               我们可以发送一个传入的hellacache请求吗
  val can_fire_hella_incoming  = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim ocntroller  //这是分配给hellashim控制器

  // Can we fire a hellacache request that the dcache nack'd  我们可以发送一个hellacache请求 that the dcache nack'd吗
  val can_fire_hella_wakeup    = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim controller  //这是分配给hellashim控制器

  //---------------------------------------------------------
  // Controller logic. Arbitrate which request actually fires          //控制器逻辑。仲裁哪个请求实际触发

  val exe_tlb_valid = Wire(Vec(memWidth, Bool()))
  for (w <- 0 until memWidth) {
    var tlb_avail  = true.B
    var dc_avail   = true.B
    var lcam_avail = true.B
    var rob_avail  = true.B

    def lsu_sched(can_fire: Bool, uses_tlb:Boolean, uses_dc:Boolean, uses_lcam: Boolean, uses_rob:Boolean): Bool = {
      val will_fire = can_fire && !(uses_tlb.B && !tlb_avail) &&
                                  !(uses_lcam.B && !lcam_avail) &&
                                  !(uses_dc.B && !dc_avail) &&
                                  !(uses_rob.B && !rob_avail)
      tlb_avail  = tlb_avail  && !(will_fire && uses_tlb.B)
      lcam_avail = lcam_avail && !(will_fire && uses_lcam.B)
      dc_avail   = dc_avail   && !(will_fire && uses_dc.B)
      rob_avail  = rob_avail  && !(will_fire && uses_rob.B)
      dontTouch(will_fire) // dontTouch these so we can inspect the will_fire signals    //不要触摸这些，这样我们就可以检查will_fire信号
      will_fire
    }
    //当要使用tlb\lcam\dcache\rob时，这些器件可用才will_fire为true

    // The order of these statements is the priority
    // Some restrictions
    //  - Incoming ops must get precedence, can't backpresure memaddrgen
    //  - Incoming hellacache ops must get precedence over retrying ops (PTW must get precedence over retrying translation)
    // Notes on performance
    //  - Prioritize releases, this speeds up cache line writebacks and refills
    //  - Store commits are lowest priority, since they don't "block" younger instructions unless stq fills up
    //这些语句的顺序是优先级
    //一些限制
    //    -传入的ops必须获得优先级，不能backpresure memaddrgen
    //    -hellacache ops必须优先于retry ops (PTW必须优先于retry 翻译)
    //性能说明
    //    -优先释放，这加快了缓存行写回和重新填充
    //    -Store提交是最低优先级的，因为它们不会阻塞年轻的指令，除非stq被填满
    //  DC：DCache
    will_fire_load_incoming (w) := lsu_sched(can_fire_load_incoming (w) , true , true , true , false)  // TLB , DC , LCAM
    will_fire_stad_incoming (w) := lsu_sched(can_fire_stad_incoming (w) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_sta_incoming  (w) := lsu_sched(can_fire_sta_incoming  (w) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_std_incoming  (w) := lsu_sched(can_fire_std_incoming  (w) , false, false, false, true)  //                 , ROB
    will_fire_sfence        (w) := lsu_sched(can_fire_sfence        (w) , true , false, false, true)  // TLB ,    ,      , ROB
    will_fire_release       (w) := lsu_sched(can_fire_release       (w) , false, false, true , false) //            LCAM
    will_fire_hella_incoming(w) := lsu_sched(can_fire_hella_incoming(w) , true , true , false, false) // TLB , DC
    will_fire_hella_wakeup  (w) := lsu_sched(can_fire_hella_wakeup  (w) , false, true , false, false) //     , DC
    will_fire_load_retry    (w) := lsu_sched(can_fire_load_retry    (w) , true , true , true , false) // TLB , DC , LCAM
    will_fire_sta_retry     (w) := lsu_sched(can_fire_sta_retry     (w) , true , false, true , true)  // TLB ,    , LCAM , ROB // TODO: This should be higher priority
    will_fire_load_wakeup   (w) := lsu_sched(can_fire_load_wakeup   (w) , false, true , true , false) //     , DC , LCAM1
    will_fire_store_commit  (w) := lsu_sched(can_fire_store_commit  (w) , false, true , false, false) //     , DC


    //assert(!(exe_req(w).valid && !(will_fire_load_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_incoming(w) || will_fire_std_incoming(w) || will_fire_sfence(w))))

    when (will_fire_load_wakeup(w)) {           //将发出load唤醒、将发出load传入、将发出load retry时，将对应的block_load_mask设为true
      block_load_mask(ldq_wakeup_idx)           := true.B
    } .elsewhen (will_fire_load_incoming(w)) {
      block_load_mask(exe_req(w).bits.uop.ldq_idx) := true.B
    } .elsewhen (will_fire_load_retry(w)) {
      block_load_mask(ldq_retry_idx)            := true.B
    }
    exe_tlb_valid(w) := !tlb_avail
  }
  assert((memWidth == 1).B ||
    (!(will_fire_sfence.reduce(_||_) && !will_fire_sfence.reduce(_&&_)) &&
     !will_fire_hella_incoming.reduce(_&&_) &&
     !will_fire_hella_wakeup.reduce(_&&_)   &&
     !will_fire_load_retry.reduce(_&&_)     &&
     !will_fire_sta_retry.reduce(_&&_)      &&
     !will_fire_store_commit.reduce(_&&_)   &&
     !will_fire_load_wakeup.reduce(_&&_)),
    "Some operations is proceeding down multiple pipes")
  //一些操作是通过多个管道进行的

  require(memWidth <= 2)

  //--------------------------------------------
  // TLB Access

  //不支持通过hella接口的SFENCE   //M_SFENCE: flush TLB
  assert(!(hella_state =/= h_ready && hella_req.cmd === rocket.M_SFENCE),
    "SFENCE through hella interface not supported")

  val exe_tlb_uop = widthMap(w =>
                    Mux(will_fire_load_incoming (w) ||
                        will_fire_stad_incoming (w) ||
                        will_fire_sta_incoming  (w) ||
                        will_fire_sfence        (w)  , exe_req(w).bits.uop,
                    Mux(will_fire_load_retry    (w)  , ldq_retry_e.bits.uop,
                    Mux(will_fire_sta_retry     (w)  , stq_retry_e.bits.uop,
                    Mux(will_fire_hella_incoming(w)  , NullMicroOp,
                                                       NullMicroOp)))))     //exe阶段进入tlb的微操作

 /*for(w <- 0 until memWidth){
   val xx = (0 until numStqEntries).map{ i => ((1.U << i) & next_live_store_mask) =/= 0.U && stq(i).valid && (io.core.st_risk_table(stq(i).bits.uop.prs1) || io.core.risk_table(stq(i).bits.uop.prs1)) && stq(i).bits.uop.lrs1_rtype === RT_FIX  }.reduce(_||_) === 0.U
   ldq(ld_enq_idx).bits.store_risk      := xx
  }*/



  val exe_tlb_vaddr = widthMap(w =>
                    Mux(will_fire_load_incoming (w) ||
                        will_fire_stad_incoming (w) ||
                        will_fire_sta_incoming  (w)  , exe_req(w).bits.addr,
                    Mux(will_fire_sfence        (w)  , exe_req(w).bits.sfence.bits.addr,
                    Mux(will_fire_load_retry    (w)  , ldq_retry_e.bits.addr.bits,
                    Mux(will_fire_sta_retry     (w)  , stq_retry_e.bits.addr.bits,
                    Mux(will_fire_hella_incoming(w)  , hella_req.addr,
                                                       0.U))))))           //exe阶段进入tlb的微地址

  val exe_sfence = WireInit((0.U).asTypeOf(Valid(new rocket.SFenceReq)))   //exe sfence
  for (w <- 0 until memWidth) {
    when (will_fire_sfence(w)) {
      exe_sfence := exe_req(w).bits.sfence
    }
  }

  val exe_size   = widthMap(w =>
                   Mux(will_fire_load_incoming (w) ||
                       will_fire_stad_incoming (w) ||
                       will_fire_sta_incoming  (w) ||
                       will_fire_sfence        (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_sta_retry     (w)  , exe_tlb_uop(w).mem_size,
                   Mux(will_fire_hella_incoming(w)  , hella_req.size,
                                                      0.U)))               //exe的size
  val exe_cmd    = widthMap(w =>
                   Mux(will_fire_load_incoming (w) ||
                       will_fire_stad_incoming (w) ||
                       will_fire_sta_incoming  (w) ||
                       will_fire_sfence        (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_sta_retry     (w)  , exe_tlb_uop(w).mem_cmd,
                   Mux(will_fire_hella_incoming(w)  , hella_req.cmd,
                                                      0.U)))              //mem_cmd:同步原语/缓存刷新

  val exe_passthr= widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , hella_req.phys,
                                                      false.B))
  val exe_kill   = widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , io.hellacache.s1_kill,
                                                      false.B))          //删除前一个周期的req
  for (w <- 0 until memWidth) {                                          //dtlb.io赋值
    dtlb.io.req(w).valid            := exe_tlb_valid(w)
    dtlb.io.req(w).bits.vaddr       := exe_tlb_vaddr(w)
    dtlb.io.req(w).bits.size        := exe_size(w)
    dtlb.io.req(w).bits.cmd         := exe_cmd(w)
    dtlb.io.req(w).bits.passthrough := exe_passthr(w)
   
    //val st = (0 until 8).map{k => (((ldq(k).valid && ldq(k).bits.addr.valid && !ldq(k).bits.addr_is_virtual && (ldq(k).bits.executed || ldq(k).bits.succeeded)) << exe_tlb_uop(w).stq_idx) & ldq(k).bits.st_dep_mask) =/= 0.U}.reduce(_||_)
    //dtlb.io.store_risk(w)           := (0 until numStqEntries).map{ k => (will_fire_load_incoming(w)) && ldq(exe_tlb_uop(w).ldq_idx).valid && ((1.U << k) & ldq(exe_tlb_uop(w).ldq_idx).bits.st_dep_mask) =/= 0.U && stq(k).valid && (xx(k) || yy (k)) && stq(k).bits.uop.lrs1_rtype === RT_FIX  }.reduce(_||_)
    dtlb.io.store_risk(w)        :=false.B //     := (will_fire_stad_incoming(w) || will_fire_sta_incoming(w) || will_fire_sta_retry(w)) && (io.core.risk_table(exe_tlb_uop(w).prs1) || io.core.st_risk_table(exe_tlb_uop(w).prs1)) && exe_tlb_uop(w).uses_stq && exe_tlb_uop(w).lrs1_rtype === RT_FIX && st
    
    dtlb.io.store_risk_tlb(w)       := (will_fire_stad_incoming(w) || will_fire_sta_incoming(w) || will_fire_sta_retry(w)) && (io.core.risk_table(exe_tlb_uop(w).prs1) || io.core.st_risk_table(exe_tlb_uop(w).prs1)) && exe_tlb_uop(w).uses_stq && exe_tlb_uop(w).lrs1_rtype === RT_FIX 
    
    val pdst_risk = Mux(exe_tlb_uop(w).dst_rtype === RT_FIX, io.core.risk_table(exe_tlb_uop(w).pdst), Mux(exe_tlb_uop(w).dst_rtype === RT_FLT, io.core.fp_risk_table(exe_tlb_uop(w).pdst), false.B) )
    val prs1_risk = Mux(exe_tlb_uop(w).lrs1_rtype === RT_FIX, io.core.risk_table(exe_tlb_uop(w).prs1), Mux(exe_tlb_uop(w).lrs1_rtype === RT_FLT, io.core.fp_risk_table(exe_tlb_uop(w).prs1), false.B) )
    val prs2_risk = Mux(exe_tlb_uop(w).lrs2_rtype === RT_FIX, io.core.risk_table(exe_tlb_uop(w).prs2), Mux(exe_tlb_uop(w).lrs2_rtype === RT_FLT, io.core.fp_risk_table(exe_tlb_uop(w).prs2), false.B) )
    val prs3_risk = Mux(exe_tlb_uop(w).frs3_en, io.core.fp_risk_table(exe_tlb_uop(w).prs3), false.B)
    val pdst_risk_st = Mux(exe_tlb_uop(w).dst_rtype === RT_FIX, io.core.st_risk_table(exe_tlb_uop(w).pdst), Mux(exe_tlb_uop(w).dst_rtype === RT_FLT, io.core.st_fp_risk_table(exe_tlb_uop(w).pdst), false.B) )
    val prs1_risk_st = Mux(exe_tlb_uop(w).lrs1_rtype === RT_FIX, io.core.st_risk_table(exe_tlb_uop(w).prs1), Mux(exe_tlb_uop(w).lrs1_rtype === RT_FLT, io.core.st_fp_risk_table(exe_tlb_uop(w).prs1), false.B) )
    val prs2_risk_st = Mux(exe_tlb_uop(w).lrs2_rtype === RT_FIX, io.core.st_risk_table(exe_tlb_uop(w).prs2), Mux(exe_tlb_uop(w).lrs2_rtype === RT_FLT, io.core.st_fp_risk_table(exe_tlb_uop(w).prs2), false.B) )
    val prs3_risk_st = Mux(exe_tlb_uop(w).frs3_en, io.core.st_fp_risk_table(exe_tlb_uop(w).prs3), false.B)
    
    dtlb.io.load_risk(w)             := (will_fire_load_incoming(w) || will_fire_load_retry(w)) && exe_tlb_uop(w).uses_ldq && (( (prs1_risk || prs2_risk || prs3_risk) && pdst_risk ) || ( (prs1_risk_st || prs2_risk_st || prs3_risk_st) && pdst_risk_st ))
    dtlb.io.all_load_risk(w)         := (will_fire_load_incoming(w) || will_fire_load_retry(w)) && exe_tlb_uop(w).uses_ldq && (pdst_risk || pdst_risk_st)
    dtlb.io.fireloadincoming := will_fire_load_incoming(w)
    dtlb.io.fireloadretry := will_fire_load_retry(w)
    dtlb.io.debug_pc := exe_tlb_uop(w).debug_pc
    dtlb.io.prs1_risk := prs1_risk
    dtlb.io.prs1_risk_st := prs1_risk_st 
 }
  
  
  
  
  dtlb.io.kill                      := exe_kill.reduce(_||_)
  dtlb.io.sfence                    := exe_sfence

  // exceptions    tlb异常
  val ma_ld = widthMap(w => will_fire_load_incoming(w) && exe_req(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc   //我们在内存地址计算中得到ma_ld
  val ma_st = widthMap(w => (will_fire_sta_incoming(w) || will_fire_stad_incoming(w)) && exe_req(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val pf_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.ld && exe_tlb_uop(w).uses_ldq)
  val pf_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.st && exe_tlb_uop(w).uses_stq)
  val ae_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.ld && exe_tlb_uop(w).uses_ldq)
  val ae_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.st && exe_tlb_uop(w).uses_stq)

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valids = RegNext(widthMap(w =>
                     (pf_ld(w) || pf_st(w) || ae_ld(w) || ae_st(w) || ma_ld(w) || ma_st(w)) &&
                     !io.core.exception &&
                     !IsKilledByBranch(io.core.brupdate, exe_tlb_uop(w))))  //内存异常有效性
  val mem_xcpt_uops   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, exe_tlb_uop(w))))         //内存异常微操作
  val mem_xcpt_causes = RegNext(widthMap(w =>
    Mux(ma_ld(w), rocket.Causes.misaligned_load.U,
    Mux(ma_st(w), rocket.Causes.misaligned_store.U,
    Mux(pf_ld(w), rocket.Causes.load_page_fault.U,
    Mux(pf_st(w), rocket.Causes.store_page_fault.U,
    Mux(ae_ld(w), rocket.Causes.load_access.U,
                  rocket.Causes.store_access.U)))))))  //内存异常原因
  val mem_xcpt_vaddrs = RegNext(exe_tlb_vaddr)         //内存异常虚拟地址

  for (w <- 0 until memWidth) {
    //// Fence假装在和TLB 通话
    assert (!(dtlb.io.req(w).valid && exe_tlb_uop(w).is_fence), "Fence is pretending to talk to the TLB")
    //// 不是加载或存储地址的uop抛出内存 异常
    assert (!((will_fire_load_incoming(w) || will_fire_sta_incoming(w) || will_fire_stad_incoming(w)) &&
      exe_req(w).bits.mxcpt.valid && dtlb.io.req(w).valid &&
    !(exe_tlb_uop(w).ctrl.is_load || exe_tlb_uop(w).ctrl.is_sta)),
      "A uop that's not a load or store-address is throwing a memory exception.")
  }

  mem_xcpt_valid := mem_xcpt_valids.reduce(_||_)
  mem_xcpt_cause := mem_xcpt_causes(0)
  mem_xcpt_uop   := mem_xcpt_uops(0)
  mem_xcpt_vaddr := mem_xcpt_vaddrs(0)
  var xcpt_found = mem_xcpt_valids(0)
  var oldest_xcpt_rob_idx = mem_xcpt_uops(0).rob_idx
  for (w <- 1 until memWidth) {                        //寻找最老的内存异常
    val is_older = WireInit(false.B)
    when (mem_xcpt_valids(w) &&
      (IsOlder(mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx, io.core.rob_head_idx) || !xcpt_found)) {
      is_older := true.B
      mem_xcpt_cause := mem_xcpt_causes(w)
      mem_xcpt_uop   := mem_xcpt_uops(w)
      mem_xcpt_vaddr := mem_xcpt_vaddrs(w)
    }
    xcpt_found = xcpt_found || mem_xcpt_valids(w)
    oldest_xcpt_rob_idx = Mux(is_older, mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx)
  }

  val exe_tlb_miss  = widthMap(w => dtlb.io.req(w).valid && (dtlb.io.resp(w).miss || !dtlb.io.req(w).ready))    //tlb miss
  val exe_tlb_paddr = widthMap(w => Cat(dtlb.io.resp(w).paddr(paddrBits-1,corePgIdxBits),
                                        exe_tlb_vaddr(w)(corePgIdxBits-1,0)))         //tlb物理地址
  val exe_tlb_uncacheable = widthMap(w => !(dtlb.io.resp(w).cacheable))               //tlb不可缓存



  for (w <- 0 until memWidth) {
    //paddrs应该匹配
    assert (exe_tlb_paddr(w) === dtlb.io.resp(w).paddr || exe_req(w).bits.sfence.valid, "[lsu] paddrs should match.")

    when (mem_xcpt_valids(w))           //当内存异常有效时，将ldq\stq微操作的异常位赋值
    {
      assert(RegNext(will_fire_load_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_incoming(w) ||
        will_fire_load_retry(w) || will_fire_sta_retry(w)))
      // Technically only faulting AMOs need this   从技术上讲，只有故障的AMO才需要这个
      assert(mem_xcpt_uops(w).uses_ldq ^ mem_xcpt_uops(w).uses_stq)
      when (mem_xcpt_uops(w).uses_ldq)
      {
        ldq(mem_xcpt_uops(w).ldq_idx).bits.uop.exception := true.B
      }
        .otherwise
      {
        stq(mem_xcpt_uops(w).stq_idx).bits.uop.exception := true.B
      }
    }
  }



  //------------------------------
  // Issue Someting to Memory           向内存中发布一些东西，相当于mem阶段
  //
  // A memory op can come from many different places
  // The address either was freshly translated, or we are
  // reading a physical address from the LDQ,STQ, or the HellaCache adapter
  // 内存op可以来自很多不同的地方
  // 地址要么是新翻译的，要么我们正在从LDQ,STQ或HellaCache适配器读取物理地址

  // defaults
  io.dmem.brupdate         := io.core.brupdate
  io.dmem.exception      := io.core.exception
  io.dmem.rob_head_idx   := io.core.rob_head_idx
  io.dmem.rob_pnr_idx    := io.core.rob_pnr_idx

  io.dmem.idle_cycles := io.core.idle_cycles

  val dmem_req = Wire(Vec(memWidth, Valid(new BoomDCacheReq)))
  io.dmem.req.valid := dmem_req.map(_.valid).reduce(_||_)
  io.dmem.req.bits  := dmem_req
  val dmem_req_fire = widthMap(w => dmem_req(w).valid && io.dmem.req.fire())  //指示dmem是否既就绪又有效

  val s0_executing_loads = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))



  for (w <- 0 until memWidth) {
    dmem_req(w).valid := false.B
    dmem_req(w).bits.uop   := NullMicroOp
    dmem_req(w).bits.addr  := 0.U
    dmem_req(w).bits.data  := 0.U
    dmem_req(w).bits.is_hella := false.B

    io.dmem.s1_kill(w) := false.B

    when (will_fire_load_incoming(w)) {                                      //将发出传入的load
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop(w)

      s0_executing_loads(ldq_incoming_idx(w)) := dmem_req_fire(w)
      assert(!ldq_incoming_e(w).bits.executed)
    } .elsewhen (will_fire_load_retry(w)) {                                  //将发出重试的load
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop(w)

      s0_executing_loads(ldq_retry_idx) := dmem_req_fire(w)
      assert(!ldq_retry_e.bits.executed)
    } .elsewhen (will_fire_store_commit(w)) {                                //将发出store提交
      dmem_req(w).valid         := true.B
      dmem_req(w).bits.addr     := stq_commit_e.bits.addr.bits
      dmem_req(w).bits.data     := (new freechips.rocketchip.rocket.StoreGen(
                                    stq_commit_e.bits.uop.mem_size, 0.U,
                                    stq_commit_e.bits.data.bits,
                                    coreDataBytes)).data
      dmem_req(w).bits.uop      := stq_commit_e.bits.uop

      stq_execute_head                     := Mux(dmem_req_fire(w),
                                                WrapInc(stq_execute_head, numStqEntries),
                                                stq_execute_head)

      stq(stq_execute_head).bits.succeeded := false.B              //succeeded表示得手了，不用再维持
    } .elsewhen (will_fire_load_wakeup(w)) {                                //将发出load唤醒
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := ldq_wakeup_e.bits.addr.bits
      dmem_req(w).bits.uop   := ldq_wakeup_e.bits.uop

      s0_executing_loads(ldq_wakeup_idx) := dmem_req_fire(w)

      assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.addr_is_virtual)
    } .elsewhen (will_fire_hella_incoming(w)) {                             //将发出传入的hella
      assert(hella_state === h_s1)

      dmem_req(w).valid               := !io.hellacache.s1_kill && (!exe_tlb_miss(w) || hella_req.phys)
      dmem_req(w).bits.addr           := exe_tlb_paddr(w)
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        io.hellacache.s1_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B

      hella_paddr := exe_tlb_paddr(w)
    }
      .elsewhen (will_fire_hella_wakeup(w))
    {                                                                      //将发出hella唤醒
      assert(hella_state === h_replay)
      dmem_req(w).valid               := true.B
      dmem_req(w).bits.addr           := hella_paddr
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        hella_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B
    }


    when(dmem_req(w).valid && dmem_req(w).bits.uop.debug_pc(31,0) === 0x00082022L.U){
      //printf(p" cycles=${io.core.idle_cycles} ")
      //printf(p" rob_idx=${dmem_req(w).bits.uop.rob_idx} ")
      //printf(p" ldq_idx=${dmem_req(w).bits.uop.ldq_idx} ")
      //printf(p"find lsu dmem(cache)_req-uop-enter 00082022\n")
    }

    //-------------------------------------------------------------
    // Write Addr into the LAQ/SAQ                 把地址写入LAQ/SAQ
    // STA UOP计算存储地址并更新STQ条目中的地址。STD UOP将存储数据移动到STQ条目中
    when (will_fire_load_incoming(w) || will_fire_load_retry(w))           //将发出load(incoming、retry)
    {
      val ldq_idx = Mux(will_fire_load_incoming(w), ldq_incoming_idx(w), ldq_retry_idx)
      ldq(ldq_idx).bits.addr.valid          := true.B
      ldq(ldq_idx).bits.addr.bits           := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      ldq(ldq_idx).bits.uop.pdst            := exe_tlb_uop(w).pdst
      ldq(ldq_idx).bits.addr_is_virtual     := exe_tlb_miss(w)              //tlb miss后，是虚拟地址；否则，是物理地址
      ldq(ldq_idx).bits.addr_is_uncacheable := exe_tlb_uncacheable(w) && !exe_tlb_miss(w)

      //传入load正在覆盖有效地址
      assert(!(will_fire_load_incoming(w) && ldq_incoming_e(w).bits.addr.valid),
        "[lsu] Incoming load is overwriting a valid address")
    }

    when (will_fire_sta_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_retry(w))      //将发出sta(d)(incoming、retry)
    {
      val stq_idx = Mux(will_fire_sta_incoming(w) || will_fire_stad_incoming(w),
        stq_incoming_idx(w), stq_retry_idx)

      stq(stq_idx).bits.addr.valid := !pf_st(w) // Prevent AMOs from executing!   阻止AMO执行!   pf:page fault
      stq(stq_idx).bits.addr.bits  := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      stq(stq_idx).bits.uop.pdst   := exe_tlb_uop(w).pdst // Needed for AMOs    AMO所需
      stq(stq_idx).bits.addr_is_virtual := exe_tlb_miss(w)

      //传入store正在覆盖有效地址
      assert(!(will_fire_sta_incoming(w) && stq_incoming_e(w).bits.addr.valid),
        "[lsu] Incoming store is overwriting a valid address")

    }

    //-------------------------------------------------------------
    // Write data into the STQ             将数据写入STQ(SDQ)
    if (w == 0)
      io.core.fp_stdata.ready := !will_fire_std_incoming(w) && !will_fire_stad_incoming(w)
    val fp_stdata_fire = io.core.fp_stdata.fire() && (w == 0).B
    when (will_fire_std_incoming(w) || will_fire_stad_incoming(w) || fp_stdata_fire)      //将发出std（stad）incoming，或浮点std
    {
      val sidx = Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
        stq_incoming_idx(w),
        io.core.fp_stdata.bits.uop.stq_idx)
      stq(sidx).bits.data.valid := true.B
      stq(sidx).bits.data.bits  := Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
        exe_req(w).bits.data,
        io.core.fp_stdata.bits.data)
      assert(!(stq(sidx).bits.data.valid),
        "[lsu] Incoming store is overwriting a valid data entry")
    }
  }
  val will_fire_stdf_incoming = io.core.fp_stdata.fire()
  require (xLen >= fLen) // for correct SDQ size

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Cache Access Cycle (Mem)            缓存访问周期(Mem)
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Note the DCache may not have accepted our request         注意，DCache可能没有接受我们的请求

  val exe_req_killed = widthMap(w => IsKilledByBranch(io.core.brupdate, exe_req(w).bits.uop)) //检测微操作是否由于分支错误预测而被杀死。
  val stdf_killed = IsKilledByBranch(io.core.brupdate, io.core.fp_stdata.bits.uop)  //检测fp std是否被分支误预测杀死

  val fired_load_incoming  = widthMap(w => RegNext(will_fire_load_incoming(w) && !exe_req_killed(w)))
  val fired_stad_incoming  = widthMap(w => RegNext(will_fire_stad_incoming(w) && !exe_req_killed(w)))
  val fired_sta_incoming   = widthMap(w => RegNext(will_fire_sta_incoming (w) && !exe_req_killed(w)))
  val fired_std_incoming   = widthMap(w => RegNext(will_fire_std_incoming (w) && !exe_req_killed(w)))
  val fired_stdf_incoming  = RegNext(will_fire_stdf_incoming && !stdf_killed)
  val fired_sfence         = RegNext(will_fire_sfence)
  val fired_release        = RegNext(will_fire_release)
  val fired_load_retry     = widthMap(w => RegNext(will_fire_load_retry   (w) && !IsKilledByBranch(io.core.brupdate, ldq_retry_e.bits.uop)))
  val fired_sta_retry      = widthMap(w => RegNext(will_fire_sta_retry    (w) && !IsKilledByBranch(io.core.brupdate, stq_retry_e.bits.uop)))
  val fired_store_commit   = RegNext(will_fire_store_commit)
  val fired_load_wakeup    = widthMap(w => RegNext(will_fire_load_wakeup  (w) && !IsKilledByBranch(io.core.brupdate, ldq_wakeup_e.bits.uop)))
  val fired_hella_incoming = RegNext(will_fire_hella_incoming)
  val fired_hella_wakeup   = RegNext(will_fire_hella_wakeup)

  val mem_incoming_uop     = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, exe_req(w).bits.uop)))   //mem将来的微操作
  val mem_ldq_incoming_e   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))))     //mem将来的ldq微操作
  val mem_stq_incoming_e   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, stq_incoming_e(w))))     //mem将来的stq微操作
  val mem_ldq_wakeup_e     = RegNext(UpdateBrMask(io.core.brupdate, ldq_wakeup_e))                         //mem唤醒的ldq微操作
  val mem_ldq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, ldq_retry_e))                          //mem重试的ldq微操作
  val mem_stq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, stq_retry_e))                          //mem重试的stq微操作
  val mem_ldq_e            = widthMap(w =>
                             Mux(fired_load_incoming(w), mem_ldq_incoming_e(w),
                             Mux(fired_load_retry   (w), mem_ldq_retry_e,
                             Mux(fired_load_wakeup  (w), mem_ldq_wakeup_e, (0.U).asTypeOf(Valid(new LDQEntry))))))   //mem ldq操作
  val mem_stq_e            = widthMap(w =>
                             Mux(fired_stad_incoming(w) ||
                                 fired_sta_incoming (w), mem_stq_incoming_e(w),
                             Mux(fired_sta_retry    (w), mem_stq_retry_e, (0.U).asTypeOf(Valid(new STQEntry)))))     //mem stq操作
  val mem_stdf_uop         = RegNext(UpdateBrMask(io.core.brupdate, io.core.fp_stdata.bits.uop))                     //mem 浮点std操作


  val mem_tlb_miss             = RegNext(exe_tlb_miss)             //mem tlb miss
  val mem_tlb_uncacheable      = RegNext(exe_tlb_uncacheable)       //mem tlb不可缓存
  val mem_paddr                = RegNext(widthMap(w => dmem_req(w).bits.addr))      //mem物理地址

  // Task 1: Clr ROB busy bit         清除ROB繁忙位
  val clr_bsy_valid   = RegInit(widthMap(w => false.B))
  val clr_bsy_rob_idx = Reg(Vec(memWidth, UInt(robAddrSz.W)))
  val clr_bsy_brmask  = Reg(Vec(memWidth, UInt(maxBrCount.W)))     //maxBrCount:我们可以同时推测的分支数

  for (w <- 0 until memWidth) {
    clr_bsy_valid   (w) := false.B
    clr_bsy_rob_idx (w) := 0.U
    clr_bsy_brmask  (w) := 0.U


    when (fired_stad_incoming(w)) {                                         //发出传入的stad
      clr_bsy_valid   (w) := mem_stq_incoming_e(w).valid           &&
                            !mem_tlb_miss(w)                       &&
                            !mem_stq_incoming_e(w).bits.uop.is_amo &&
                            !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask  (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
    } .elsewhen (fired_sta_incoming(w)) {                                   //发出传入的sta
      clr_bsy_valid   (w) := mem_stq_incoming_e(w).valid            &&
                             mem_stq_incoming_e(w).bits.data.valid  &&
                            !mem_tlb_miss(w)                        &&
                            !mem_stq_incoming_e(w).bits.uop.is_amo  &&
                            !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask  (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
    } .elsewhen (fired_std_incoming(w)) {                                   //发出传入的std
      clr_bsy_valid   (w) := mem_stq_incoming_e(w).valid                 &&
                             mem_stq_incoming_e(w).bits.addr.valid       &&
                            !mem_stq_incoming_e(w).bits.addr_is_virtual  &&
                            !mem_stq_incoming_e(w).bits.uop.is_amo       &&
                            !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask  (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
    } .elsewhen (fired_sfence(w)) {                                         //发出sfence
      clr_bsy_valid   (w) := (w == 0).B // SFence proceeds down all paths, only allow one to clr the rob
      clr_bsy_rob_idx (w) := mem_incoming_uop(w).rob_idx
      clr_bsy_brmask  (w) := GetNewBrMask(io.core.brupdate, mem_incoming_uop(w))
    } .elsewhen (fired_sta_retry(w)) {                                      //发出重试的sta
      clr_bsy_valid   (w) := mem_stq_retry_e.valid            &&
                             mem_stq_retry_e.bits.data.valid  &&
                            !mem_tlb_miss(w)                  &&
                            !mem_stq_retry_e.bits.uop.is_amo  &&
                            !IsKilledByBranch(io.core.brupdate, mem_stq_retry_e.bits.uop)
      clr_bsy_rob_idx (w) := mem_stq_retry_e.bits.uop.rob_idx
      clr_bsy_brmask  (w) := GetNewBrMask(io.core.brupdate, mem_stq_retry_e.bits.uop)
    }


    io.core.clr_bsy(w).valid := clr_bsy_valid(w) &&
                               !IsKilledByBranch(io.core.brupdate, clr_bsy_brmask(w)) &&
                               !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
    io.core.clr_bsy(w).bits  := clr_bsy_rob_idx(w)
  }

  //清除浮点std的rob busy位
  val stdf_clr_bsy_valid   = RegInit(false.B)
  val stdf_clr_bsy_rob_idx = Reg(UInt(robAddrSz.W))
  val stdf_clr_bsy_brmask  = Reg(UInt(maxBrCount.W))
  stdf_clr_bsy_valid   := false.B
  stdf_clr_bsy_rob_idx := 0.U
  stdf_clr_bsy_brmask  := 0.U
  when (fired_stdf_incoming) {
    val s_idx = mem_stdf_uop.stq_idx
    stdf_clr_bsy_valid   := stq(s_idx).valid                 &&
                            stq(s_idx).bits.addr.valid       &&
                            !stq(s_idx).bits.addr_is_virtual &&
                            !stq(s_idx).bits.uop.is_amo      &&
                            !IsKilledByBranch(io.core.brupdate, mem_stdf_uop)
    stdf_clr_bsy_rob_idx := mem_stdf_uop.rob_idx
    stdf_clr_bsy_brmask  := GetNewBrMask(io.core.brupdate, mem_stdf_uop)
  }



  io.core.clr_bsy(memWidth).valid := stdf_clr_bsy_valid &&
                                    !IsKilledByBranch(io.core.brupdate, stdf_clr_bsy_brmask) &&
                                    !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
  io.core.clr_bsy(memWidth).bits  := stdf_clr_bsy_rob_idx



  // Task 2: Do LD-LD. ST-LD searches for ordering failures
  //         Do LD-ST search for forwarding opportunities
  // We have the opportunity to kill a request we sent last cycle. Use it wisely!
  // Task 2: 做LD-LD、、ST-LD搜索 排序失败
  // 做LD-ST搜索 转发机会
  // 我们有机会杀死上一个循环发送的请求。明智的使用它!

  // We translated a store last cycle          //我们在上一个周期中翻译了一个store
  val do_st_search = widthMap(w => (fired_stad_incoming(w) || fired_sta_incoming(w) || fired_sta_retry(w)) && !mem_tlb_miss(w))
  // We translated a load last cycle           //上个周期我们翻译了一个load
  val do_ld_search = widthMap(w => ((fired_load_incoming(w) || fired_load_retry(w)) && !mem_tlb_miss(w)) ||
                     fired_load_wakeup(w))
  // We are making a local line visible to other harts    我们正在让其他harts看到本地行
  val do_release_search = widthMap(w => fired_release(w))

  // Store addrs don't go to memory yet, get it from the TLB response    store addr还没有进入内存，从TLB响应中获取它
  // Load wakeups don't go through TLB, get it through memory            load唤醒不会通过TLB，而是通过内存获得它
  // Load incoming and load retries go through both                      load传入和load重试都通过这两种方式

  val lcam_addr  = widthMap(w => Mux(fired_stad_incoming(w) || fired_sta_incoming(w) || fired_sta_retry(w),
                                     RegNext(exe_tlb_paddr(w)),
                                     Mux(fired_release(w), RegNext(io.dmem.release.bits.address),
                                         mem_paddr(w))))                                           //cam搜索地址
  val lcam_uop   = widthMap(w => Mux(do_st_search(w), mem_stq_e(w).bits.uop,
                                 Mux(do_ld_search(w), mem_ldq_e(w).bits.uop, NullMicroOp)))        //cam搜索微操作

  val lcam_mask  = widthMap(w => GenByteMask(lcam_addr(w), lcam_uop(w).mem_size))                  //cam掩码 （搜索地址）  //GenByteMask:获取一个地址并生成一个8位掩码，该掩码包含双字中的字节。
  val lcam_st_dep_mask = widthMap(w => mem_ldq_e(w).bits.st_dep_mask)                              //cam load前更老的store掩码
  val lcam_is_release = widthMap(w => fired_release(w))                                            //cam is release
  val lcam_ldq_idx  = widthMap(w =>
                      Mux(fired_load_incoming(w), mem_incoming_uop(w).ldq_idx,
                      Mux(fired_load_wakeup  (w), RegNext(ldq_wakeup_idx),
                      Mux(fired_load_retry   (w), RegNext(ldq_retry_idx), 0.U))))                  //cam搜索 ldq索引
  val lcam_stq_idx  = widthMap(w =>
                      Mux(fired_stad_incoming(w) ||
                          fired_sta_incoming (w), mem_incoming_uop(w).stq_idx,
                      Mux(fired_sta_retry    (w), RegNext(stq_retry_idx), 0.U)))                   //cam搜索 stq索引

  val can_forward = WireInit(widthMap(w =>
    Mux(fired_load_incoming(w) || fired_load_retry(w), !mem_tlb_uncacheable(w),
      !ldq(lcam_ldq_idx(w)).bits.addr_is_uncacheable)))                                            //可以直达，不可缓存

  // Mask of stores which we conflict on address with                                              //在地址上冲突的store的掩码
  val ldst_addr_matches    = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))
  // Mask of stores which we can forward from                                                      //store的掩码，我们可以转发
  val ldst_forward_matches = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))

  //我们将报告为失败的load(抛出一个mini异常)
  val failed_loads     = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B))) // Loads which we will report as failures (throws a mini-exception)
  //下一个阶段被dcache未应答的负载
  val nacking_loads    = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B))) // Loads which are being nacked by dcache in the next stage

  val s1_executing_loads = RegNext(s0_executing_loads)         //s0_executing_loads: dmem_req_fire
  val s1_set_execute     = WireInit(s1_executing_loads)

  val mem_forward_valid   = Wire(Vec(memWidth, Bool()))      //mem 转发有效
  val mem_forward_ldq_idx = lcam_ldq_idx                     //mem 转发ldq索引
  val mem_forward_ld_addr = lcam_addr                        //mem 转发load地址
  val mem_forward_stq_idx = Wire(Vec(memWidth, UInt(log2Ceil(numStqEntries).W)))   //mem 转发stq索引

  val wb_forward_valid    = RegNext(mem_forward_valid)       //写回 转发有效
  val wb_forward_ldq_idx  = RegNext(mem_forward_ldq_idx)     //写回 转发ldq索引
  val wb_forward_ld_addr  = RegNext(mem_forward_ld_addr)     //写回 转发load地址
  val wb_forward_stq_idx  = RegNext(mem_forward_stq_idx)     //写回 转发stq索引

  for(i <- 0 until numStqEntries){
     when(stq(i).valid && stq(i).bits.preorder_fail =/= 9.U && !io.core.risk_table(stq(i).bits.uop.prs1) && !io.core.st_risk_table(stq(i).bits.uop.prs1)){
         when(ldq(stq(i).bits.preorder_fail).bits.uop.orderfailstq === i.U){
             ldq(stq(i).bits.preorder_fail).bits.order_fail := true.B
             failed_loads(stq(i).bits.preorder_fail) := true.B
             stq(i).bits.preorder_fail := 9.U
             ldq(stq(i).bits.preorder_fail).bits.uop.orderfailstq := 9.U
             printf(p"searcher is older end")
             printf("pc=0x%x \n", stq(i).bits.uop.debug_pc)

         }
     }

  }

  
  for (i <- 0 until numLdqEntries) {


    val pdst_risk1 = Mux(ldq(i).bits.uop.dst_rtype === RT_FIX, io.core.risk_table(ldq(i).bits.uop.pdst), Mux(ldq(i).bits.uop.dst_rtype === RT_FLT, io.core.fp_risk_table(ldq(i).bits.uop.pdst), false.B) )
    val prs1_risk1 = Mux(ldq(i).bits.uop.lrs1_rtype === RT_FIX, io.core.risk_table(ldq(i).bits.uop.prs1), Mux(ldq(i).bits.uop.lrs1_rtype === RT_FLT, io.core.fp_risk_table(ldq(i).bits.uop.prs1), false.B) )
    val prs2_risk1 = Mux(ldq(i).bits.uop.lrs2_rtype === RT_FIX, io.core.risk_table(ldq(i).bits.uop.prs2), Mux(ldq(i).bits.uop.lrs2_rtype === RT_FLT, io.core.fp_risk_table(ldq(i).bits.uop.prs2), false.B) )
    val prs3_risk1 = Mux(ldq(i).bits.uop.frs3_en, io.core.fp_risk_table(ldq(i).bits.uop.prs3), false.B)
    val pdst_risk_st1 = Mux(ldq(i).bits.uop.dst_rtype === RT_FIX, io.core.st_risk_table(ldq(i).bits.uop.pdst), Mux(ldq(i).bits.uop.dst_rtype === RT_FLT, io.core.st_fp_risk_table(ldq(i).bits.uop.pdst), false.B) )
    val prs1_risk_st1 = Mux(ldq(i).bits.uop.lrs1_rtype === RT_FIX, io.core.st_risk_table(ldq(i).bits.uop.prs1), Mux(ldq(i).bits.uop.lrs1_rtype === RT_FLT, io.core.st_fp_risk_table(ldq(i).bits.uop.prs1), false.B) )
    val prs2_risk_st1 = Mux(ldq(i).bits.uop.lrs2_rtype === RT_FIX, io.core.st_risk_table(ldq(i).bits.uop.prs2), Mux(ldq(i).bits.uop.lrs2_rtype === RT_FLT, io.core.st_fp_risk_table(ldq(i).bits.uop.prs2), false.B) )
    val prs3_risk_st1 = Mux(ldq(i).bits.uop.frs3_en, io.core.st_fp_risk_table(ldq(i).bits.uop.prs3), false.B)

    val ld1 = ( (prs1_risk1 || prs2_risk1 || prs3_risk1) && pdst_risk1 ) || ( (prs1_risk_st1 || prs2_risk_st1 || prs3_risk_st1) && pdst_risk_st1 )

       when(ldq(i).valid && ldq(i).bits.preorder_fail =/= 9.U && !ld1  ){
          when(  ldq(ldq(i).bits.preorder_fail).bits.uop.orderfailldq === i.U ){
              ldq(ldq(i).bits.preorder_fail).bits.order_fail := true.B
              failed_loads(ldq(i).bits.preorder_fail)        := true.B
              ldq(i).bits.preorder_fail := 9.U 
              ldq(ldq(i).bits.preorder_fail).bits.uop.orderfailldq := 9.U
              printf(p"searcher_is_older_end_ldq ")
              printf("pc = 0x%x \n", ldq(i).bits.uop.debug_pc)
          }
       }
       
  }



  for (i <- 0 until numLdqEntries) {           //遍历ldq条目
    //ldq第i个条目的信息
    val l_valid = ldq(i).valid
    val l_bits  = ldq(i).bits
    val l_addr  = ldq(i).bits.addr.bits
    val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

    val l_forwarders      = widthMap(w => wb_forward_valid(w) && wb_forward_ldq_idx(w) === i.U)       //写回 转发有效，且 当前写回转发ldq索引==当前ldq条目
    val l_is_forwarding   = l_forwarders.reduce(_||_)
    val l_forward_stq_idx = Mux(l_is_forwarding, Mux1H(l_forwarders, wb_forward_stq_idx), l_bits.forward_stq_idx)


    val block_addr_matches = widthMap(w => lcam_addr(w) >> blockOffBits === l_addr >> blockOffBits)   //lcam搜索地址与当前条目地址的 block偏移匹配
    val dword_addr_matches = widthMap(w => block_addr_matches(w) && lcam_addr(w)(blockOffBits-1,3) === l_addr(blockOffBits-1,3))    //dword地址匹配
    val mask_match   = widthMap(w => (l_mask & lcam_mask(w)) === l_mask)       //掩码匹配
    val mask_overlap = widthMap(w => (l_mask & lcam_mask(w)).orR)              //掩码重叠

    // Searcher is a store       Searcher是一个store
    for (w <- 0 until memWidth) {

      when (do_release_search(w) &&
            l_valid              &&
            l_bits.addr.valid    &&
            block_addr_matches(w)) {                          //release搜索，block地址匹配
        // This load has been observed, so if a younger load to the same address has not
        // executed yet, this load must be squashed
        ////这个load已经被观察到，所以如果一个更年轻的load到相同的地址还没有被执行，这个load必须被squashed
        ldq(i).bits.observed := true.B
      } .elsewhen (do_st_search(w)                                                                                                &&
                   l_valid                                                                                                        &&
                   l_bits.addr.valid                                                                                              &&
                   (l_bits.executed || l_bits.succeeded || l_is_forwarding)                                                       &&
                   !l_bits.addr_is_virtual                                                                                        &&
                   l_bits.st_dep_mask(lcam_stq_idx(w))                                                                            &&
                   dword_addr_matches(w)                                                                                          &&
                   mask_overlap(w)) {                         //st搜索，dword地址匹配

        val forwarded_is_older = IsOlder(l_forward_stq_idx, lcam_stq_idx(w), l_bits.youngest_stq_idx)
        // We are older than this load, which overlapped us.            //我们比这个load更老，它与我们重叠。
        when (!l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed   //如果load没有被转发，它肯定失败了
          ((l_forward_stq_idx =/= lcam_stq_idx(w)) && forwarded_is_older)) { // If the load forwarded from us, we might be ok  //如果load从我们这里转发，我们可能会没事
         /* when(io.core.risk_table(lcam_uop(w).prs1) || io.core.st_risk_table(lcam_uop(w).prs1)){
              printf(p"searcher is riskstore ")
              printf("pc = 0x%x \n", lcam_uop(w).debug_pc)
              stq(lcam_stq_idx(w)).bits.preorder_fail := i.U
              ldq(i).bits.uop.orderfailstq := lcam_stq_idx(w)
          }  
          .otherwise{  */
              printf(p" searcher is store ")
              printf("pc = 0x%x \n", lcam_uop(w).debug_pc)
              ldq(i).bits.order_fail := true.B
              failed_loads(i)        := true.B
         //  }
        }
      } .elsewhen (do_ld_search(w)            &&
                   l_valid                    &&
                   l_bits.addr.valid          &&
                   !l_bits.addr_is_virtual    &&
                   dword_addr_matches(w)      &&
                   mask_overlap(w)) {                         //ld搜索，dword地址匹配
        val searcher_is_older = IsOlder(lcam_ldq_idx(w), i.U, ldq_head)
        when (searcher_is_older) {
          when ((l_bits.executed || l_bits.succeeded || l_is_forwarding) &&
                !s1_executing_loads(i) && // If the load is proceeding in parallel we don't need to kill it    如果load是并行进行的，我们不需要杀死它
                l_bits.observed) {        // Its only a ordering failure if the cache line was observed between the younger load and us
                                          //如果在更年轻的load和我们之间观察到缓存行，则只有排序失败
  

    val pdst_risk = Mux(lcam_uop(w).dst_rtype === RT_FIX, io.core.risk_table(lcam_uop(w).pdst), Mux(lcam_uop(w).dst_rtype === RT_FLT, io.core.fp_risk_table(lcam_uop(w).pdst), false.B) )
    val prs1_risk = Mux(lcam_uop(w).lrs1_rtype === RT_FIX, io.core.risk_table(lcam_uop(w).prs1), Mux(lcam_uop(w).lrs1_rtype === RT_FLT, io.core.fp_risk_table(lcam_uop(w).prs1), false.B) )
    val prs2_risk = Mux(lcam_uop(w).lrs2_rtype === RT_FIX, io.core.risk_table(lcam_uop(w).prs2), Mux(lcam_uop(w).lrs2_rtype === RT_FLT, io.core.fp_risk_table(lcam_uop(w).prs2), false.B) )
    val prs3_risk = Mux(lcam_uop(w).frs3_en, io.core.fp_risk_table(lcam_uop(w).prs3), false.B)
    val pdst_risk_st = Mux(lcam_uop(w).dst_rtype === RT_FIX, io.core.st_risk_table(lcam_uop(w).pdst), Mux(lcam_uop(w).dst_rtype === RT_FLT, io.core.st_fp_risk_table(lcam_uop(w).pdst), false.B) )
    val prs1_risk_st = Mux(lcam_uop(w).lrs1_rtype === RT_FIX, io.core.st_risk_table(lcam_uop(w).prs1), Mux(lcam_uop(w).lrs1_rtype === RT_FLT, io.core.st_fp_risk_table(lcam_uop(w).prs1), false.B) )
    val prs2_risk_st = Mux(lcam_uop(w).lrs2_rtype === RT_FIX, io.core.st_risk_table(lcam_uop(w).prs2), Mux(lcam_uop(w).lrs2_rtype === RT_FLT, io.core.st_fp_risk_table(lcam_uop(w).prs2), false.B) )
    val prs3_risk_st = Mux(lcam_uop(w).frs3_en, io.core.st_fp_risk_table(lcam_uop(w).prs3), false.B)
    
    val ld = ( (prs1_risk || prs2_risk || prs3_risk) && pdst_risk ) || ( (prs1_risk_st || prs2_risk_st || prs3_risk_st) && pdst_risk_st )              
                           
                           
          /*   when( ld ){
                 printf(p"searcher_is_riskolderload ")
                 printf("pc = 0x%x \n", lcam_uop(w).debug_pc)
                 ldq(lcam_ldq_idx(w)).bits.preorder_fail := i.U
                 ldq(i).bits.uop.orderfailldq := lcam_ldq_idx(w)
             }
             .otherwise{ */
                ldq(i).bits.order_fail := true.B
                failed_loads(i)        := true.B
             // }
          }
        } .elsewhen (lcam_ldq_idx(w) =/= i.U) {
          // The load is older, and either it hasn't executed, it was nacked, or it is ignoring its response
          // we need to kill ourselves, and prevent forwarding
          // load是旧的，它没有执行，或它是nack，或它正在忽略它的响应
          // 我们需要杀死自己，并阻止转发
          val older_nacked = nacking_loads(i) || RegNext(nacking_loads(i))
          when (!(l_bits.executed || l_bits.succeeded) || older_nacked) {
            s1_set_execute(lcam_ldq_idx(w))    := false.B
            io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))   //在LSU的LCAM搜索阶段，如果排序失败(或可能转发)，杀死
            can_forward(w)                     := false.B                     //不可以转发
          }
        }
      }
    }
  }

  for(i <- 0 until numLdqEntries){
    io.core.ldq_dep_mask(i) := ldq(i).bits.st_dep_mask
    io.core.ldq_valid(i) := ldq(i).valid
  }
  for(i <- 0 until numStqEntries){
    io.core.stq_valid(i) := stq(i).valid
    io.core.stq_addr_valid(i) := stq(i).bits.addr.valid
    io.core.stq_addr_virtual(i) := stq(i).bits.addr_is_virtual
  }

  for(w <- 0 until memWidth) {
    when(exe_tlb_miss(0)) {
  /*    printf(p"tlb miss in lsu   ")
      printf("pc=0x%x ",exe_tlb_uop(0).debug_pc)
      printf("pdst=%d ",exe_tlb_uop(0).pdst)
      printf("rob_idx=%d ",exe_tlb_uop(0).rob_idx)
      printf(p"dmem_req(w).valid=${dmem_req(w).valid}   ")
      printf(p"do_ld_search=${do_ld_search(w)}   ")
      printf(p"will_fire_load_incoming =${will_fire_load_incoming}\n")
    }*/
     /*when(dmem_req(w).valid && dmem_req(w).bits.uop.debug_pc === 0x00082022L.U) {
        printf(p"rob_idx = ${dmem_req(w).bits.uop.rob_idx} ")
        printf(p"00082022 tlb miss in lsu  dmem become valid \n")
     }*/
    /* when(do_ld_search(w) && mem_ldq_e(w).valid && mem_ldq_e(w).bits.uop.debug_pc === 0x80001310L.U) {
        printf(p"rob_idx = ${mem_ldq_e(w).bits.uop.rob_idx} ")
        printf(p"80001310 tlb miss in lsu  do ld search \n")
     }
   */  
  }
}


  //推测执行时，分支地址求解并检测后，1继续执行->离开推测窗口，2回滚->也离开了推测窗口
  //推测执行时，store指令地址解析并检测有无先前load地址一样后，1一样且未转发->回滚 2继续执行->离开推测窗口
  //遍历stq，判断有无地址未解析的store
  //(!stq(i).bits.addr.valid || stq(i).bits.addr_is_virtual) 代表做st搜索开始的时刻，st搜索结束时，推测性消失
    io.core.storeaddr_not_get := (0 until numStqEntries).map{ i => stq(i).valid && (!stq(i).bits.addr.valid ) }.reduce(_||_) =/= 0.U

    io.core.store_wuran := (0 until numStqEntries).map{ i =>  stq(i).valid &&  ( (io.core.st_risk_table(stq(i).bits.uop.prs1) || io.core.risk_table(stq(i).bits.uop.prs1)) && stq(i).bits.uop.lrs1_rtype === RT_FIX)   }.reduce(_||_) =/= 0.U

    for (i <- 0 until numStqEntries) {             //遍历stq条目
    val s_addr = stq(i).bits.addr.bits
    val s_uop  = stq(i).bits.uop
    val dword_addr_matches = widthMap(w =>
                             ( stq(i).bits.addr.valid      &&
                              !stq(i).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(w)(corePAddrBits-1,3))))
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)
    for (w <- 0 until memWidth) {
      when (do_ld_search(w) && stq(i).valid && lcam_st_dep_mask(w)(i)) {       //ld搜索，stq条目有效，cam load前更老的store掩码
      
         /*when(lcam_uop(w).debug_pc === 0x80001310L.U){
              printf(p"11111111111dword_addr_matches(w)" )
              printf(p" cycles=${io.core.idle_cycles} ")
              printf(p"pc = 80001310" )
              printf(p"lcam_mask = ${((lcam_mask(w) & write_mask) === lcam_mask(w))} " )
              printf(p"can_forward = ${can_forward(w)} " )              
              printf(p"!s_uop.is_fence = ${!s_uop.is_fence} " )
              printf(p"dword_addr_matches(w) = ${dword_addr_matches(w)} \n" )
              printf(p"fired_load_incoming(w) || fired_load_retry(w) = ${fired_load_incoming(w) || fired_load_retry(w)} " )
              printf(p"!mem_tlb_uncacheable(w) = ${!mem_tlb_uncacheable(w)} " )
              printf(p"exe_tlb_uncacheable = ${exe_tlb_uncacheable(w)} " )
              printf(p"dtlb.io.resp(w).cacheable = ${!(dtlb.io.resp(w).cacheable)} " )  
              printf(p"rob_idx = ${lcam_uop(w).rob_idx} \n" )
          }*/
      
      
        when (((lcam_mask(w) & write_mask) === lcam_mask(w)) && !s_uop.is_fence && dword_addr_matches(w) && can_forward(w)) //dword地址匹配，可以转发，不是fence
        {
          ldst_addr_matches(w)(i)            := true.B                        //我们在地址上冲突的store的掩码
          ldst_forward_matches(w)(i)         := true.B                        //我们可以转发的store的掩码
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))     //设置s1 kill
          s1_set_execute(lcam_ldq_idx(w))    := false.B                       //可以转发，就不用执行了
          /*when(lcam_uop(w).debug_pc === 0x80001310L.U){
              printf(p"can_forward(w)" )
              printf(p"pc = 80001310" )
              printf(p"rob_idx = ${lcam_uop(w).rob_idx} \n" )
          }*/
        }
          .elsewhen (((lcam_mask(w) & write_mask) =/= 0.U) && dword_addr_matches(w))   //dword地址匹配，s_addr掩码与cam掩码 相与不为0
        {
          ldst_addr_matches(w)(i)            := true.B
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
          s1_set_execute(lcam_ldq_idx(w))    := false.B
          /*when(lcam_uop(w).debug_pc === 0x80001310L.U){
              printf(p"dword_addr_matches(w)" )
              printf(p" cycles=${io.core.idle_cycles} ")
              printf(p"pc = 80001310" )
              printf(p"lcam_mask = ${((lcam_mask(w) & write_mask) === lcam_mask(w))} " )
              printf(p"can_forward = ${can_forward(w)} " )
              printf(p"fired_load_incoming(w) || fired_load_retry(w) = ${fired_load_incoming(w) || fired_load_retry(w)} " )
              printf(p"!mem_tlb_uncacheable(w) = ${!mem_tlb_uncacheable(w)} " )
              printf(p"exe_tlb_uncacheable = ${exe_tlb_uncacheable(w)} " )
              printf(p"dtlb.io.resp(w).cacheable = ${!(dtlb.io.resp(w).cacheable)} " )   
              printf(p"!s_uop.is_fence = ${!s_uop.is_fence} " )
              printf(p"dword_addr_matches(w) = ${dword_addr_matches(w)} " )
              printf(p"rob_idx = ${lcam_uop(w).rob_idx} \n" )
          }*/
        }
          .elsewhen (s_uop.is_fence || s_uop.is_amo)        //是fence或amo
        {
          ldst_addr_matches(w)(i)            := true.B
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
      }
    }
  }

  // Set execute bit in LDQ                //设置LDQ中的执行位
  for (i <- 0 until numLdqEntries) {
    when (s1_set_execute(i)) { 
    ldq(i).bits.executed := true.B
    /*printf(p"executed true")
    printf("pc=0x%x ",ldq(i).bits.uop.debug_pc)
    printf(p"rob_idx=${ldq(i).bits.uop.rob_idx}\n")*/
     }
  }

  // Find the youngest store which the load is dependent on    //找到load所依赖的最年轻的store
  val forwarding_age_logic = Seq.fill(memWidth) { Module(new ForwardingAgeLogic(numStqEntries)) }
  for (w <- 0 until memWidth) {
    forwarding_age_logic(w).io.addr_matches    := ldst_addr_matches(w).asUInt
    forwarding_age_logic(w).io.youngest_st_idx := lcam_uop(w).stq_idx
  }
  val forwarding_idx = widthMap(w => forwarding_age_logic(w).io.forwarding_idx)     //load所依赖的最年轻的store

  // Forward if st-ld forwarding is possible from the writemask and loadmask        //如果可以从write掩码和load掩码进行st-ld转发，则转发
  mem_forward_valid       := widthMap(w =>
                                  (ldst_forward_matches(w)(forwarding_idx(w))        &&
                                 !IsKilledByBranch(io.core.brupdate, lcam_uop(w))    &&
                                 !io.core.exception && !RegNext(io.core.exception)))   //内存转发有效性：store可以转发、未被分支误预测杀死、没有异常信号、下一个周期没有异常
  mem_forward_stq_idx     := forwarding_idx          //mem 转发stq 索引

  // Avoid deadlock with a 1-w LSU prioritizing load wakeups > store commits
  // On a 2W machine, load wakeups and store commits occupy separate pipelines,
  // so only add this logic for 1-w LSU
  //避免死锁在1-w LSU上，优先 load wakeups > store提交
  //在2W机器上，load wakeups和store commit占用不同的管道，
  //因此仅为1-w LSU添加此逻辑
  if (memWidth == 1) {
    // Wakeups may repeatedly find a st->ld addr conflict and fail to forward,
    // repeated wakeups may block the store from ever committing
    // Disallow load wakeups 1 cycle after this happens to allow the stores to drain
    // Wakeups可能反复发现一个st->ld地址冲突而无法转发，
    // 重复的唤醒可能会阻止store提交
    // 禁用load wakeups 1循环，在此之后允许存储耗尽
    when (RegNext(ldst_addr_matches(0).reduce(_||_) && !mem_forward_valid(0))) {      //ldst_addr_matches：在地址上冲突的store的掩码  //mem_forward_valid：mem 转发有效
      block_load_wakeup := true.B
    }

    // If stores remain blocked for 15 cycles, block load wakeups to get a store through   //如果商店保持阻塞15个周期，block load wakeups以获得一个store通过
    val store_blocked_counter = Reg(UInt(4.W))
    when (will_fire_store_commit(0) || !can_fire_store_commit(0)) {
      store_blocked_counter := 0.U
    } .elsewhen (can_fire_store_commit(0) && !will_fire_store_commit(0)) {
      store_blocked_counter := Mux(store_blocked_counter === 15.U, store_blocked_counter + 1.U, 15.U)
    }
    when (store_blocked_counter === 15.U) {
      block_load_wakeup := true.B
    }
  }


  // Task 3: Clr unsafe bit in ROB for succesful translations
  //         Delay this a cycle to avoid going ahead of the exception broadcast
  //         The unsafe bit is cleared on the first translation, so no need to fire for load wakeups
  //任务3:ROB中的clear不安全位，用于成功的翻译
  //延迟this一个周期以避免在异常广播之前进行
  //不安全的位在第一次翻译时被清除，因此不需要触发load唤醒
  for (w <- 0 until memWidth) {
    io.core.clr_unsafe(w).valid := RegNext((do_st_search(w) || do_ld_search(w)) && !fired_load_wakeup(w)) && false.B //不发出load唤醒
    io.core.clr_unsafe(w).bits  := RegNext(lcam_uop(w).rob_idx)     //lcam_uop: cam搜索微操作   //do_st_search:我们在上一个周期中翻译了一个store
  }

  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  //检测哪些负载被标记为失败，但广播给ROB的是最老的失败负载
  // TODO encapsulate this in an age-based  priority-encoder
  //   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(numLdqEntries)(i => failed_loads(i) && i.U >= laq_head)
  //   ++ failed_loads)).asUInt)
  //TODO:将其封装在基于年龄的优先级编码器中
  val temp_bits = (VecInit(VecInit.tabulate(numLdqEntries)(i =>
    failed_loads(i) && i.U >= ldq_head) ++ failed_loads)).asUInt
  val l_idx = PriorityEncoder(temp_bits)

  // one exception port, but multiple causes!
  // - 1) the incoming store-address finds a faulting load (it is by definition younger)
  // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  //一个异常端口，但有多个原因!
  // - 1)传入的store地址发现一个错误load(它根据定义更年轻)  ld_xcpt_valid
  // - 2)传入的load或store地址异常。它必须是更古老的，因此必须优先。   mem_xcpt_valid
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  val ld_xcpt_valid = failed_loads.reduce(_|_)
  val ld_xcpt_uop   = ldq(Mux(l_idx >= numLdqEntries.U, l_idx - numLdqEntries.U, l_idx)).bits.uop

  val use_mem_xcpt = (mem_xcpt_valid && IsOlder(mem_xcpt_uop.rob_idx, ld_xcpt_uop.rob_idx, io.core.rob_head_idx)) || !ld_xcpt_valid

  val xcpt_uop = Mux(use_mem_xcpt, mem_xcpt_uop, ld_xcpt_uop)

  r_xcpt_valid := (ld_xcpt_valid || mem_xcpt_valid) &&
                   !io.core.exception &&
                   !IsKilledByBranch(io.core.brupdate, xcpt_uop)
  r_xcpt.uop         := xcpt_uop
  r_xcpt.uop.br_mask := GetNewBrMask(io.core.brupdate, xcpt_uop)
  r_xcpt.cause       := Mux(use_mem_xcpt, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
  r_xcpt.badvaddr    := mem_xcpt_vaddr // TODO is there another register we can use instead?

  io.core.lxcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brupdate, r_xcpt.uop)
  io.core.lxcpt.bits  := r_xcpt

  // Task 4: Speculatively wakeup loads 1 cycle before they come back    推测性唤醒负载1个周期，在它们返回之前
  for (w <- 0 until memWidth) {          //启动快速load使用
    io.core.spec_ld_wakeup(w).valid := enableFastLoadUse.B          &&
                                       fired_load_incoming(w)       &&
                                       !mem_incoming_uop(w).fp_val  &&
                                       mem_incoming_uop(w).pdst =/= 0.U
    io.core.spec_ld_wakeup(w).bits  := mem_incoming_uop(w).pdst
  }


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)            写回周期（St-> Ld转发路径）
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Handle Memory Responses and nacks                   处理内存响应和缺失
  //----------------------------------
  for (w <- 0 until memWidth) {
    io.core.exe(w).iresp.valid := false.B
    io.core.exe(w).fresp.valid := false.B
  }


  val dmem_resp_fired = WireInit(widthMap(w => false.B))



  for (w <- 0 until memWidth) {
    // Handle nacks            处理nacks
    when (io.dmem.nack(w).valid)  //dmem缺失有效
    {


        /*when(io.dmem.nack(w).bits.uop.debug_pc === 0x80001310L.U){
          printf(p" cycles=${io.core.idle_cycles} ")
          printf(" pc=0x%x ",io.dmem.nack(w).bits.uop.debug_pc)
          printf(p" rob_idx=${io.dmem.nack(w).bits.uop.rob_idx} ")
          printf(p"find lsu dmem-nack 80001310\n")
        }*/
        when(io.dmem.nack(w).bits.uop.debug_pc === 0x00082022L.U){
          //printf(p" cycles=${io.core.idle_cycles} ")
          //printf(p"find lsu dmem-nack 00082022\n")
        }

      // We have to re-execute this!      我们得重新执行!
      when (io.dmem.nack(w).bits.is_hella)          //hella的dmem缺失
      {
        assert(hella_state === h_wait || hella_state === h_dead)
      }
        .elsewhen (io.dmem.nack(w).bits.uop.uses_ldq) //使用load的dmem缺失
      {
        assert(ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed)
        ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed  := false.B      //该ldq(ldq_idx)视为未执行
        nacking_loads(io.dmem.nack(w).bits.uop.ldq_idx) := true.B            //缺失load队列对应项为true
      }
        .otherwise                                    //使用store的dmem缺失
      {
        assert(io.dmem.nack(w).bits.uop.uses_stq)
        when (IsOlder(io.dmem.nack(w).bits.uop.stq_idx, stq_execute_head, stq_head)) {
          stq_execute_head := io.dmem.nack(w).bits.uop.stq_idx         //该stq_idx更老，则stq执行头更新为它
        }
      }
    }





    // Handle the response     处理内存响应
    when (io.dmem.resp(w).valid)    //dmem返回有效
    {

        when (io.dmem.resp(w).bits.uop.uses_ldq)        //使用ldq
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        val ldq_idx = io.dmem.resp(w).bits.uop.ldq_idx
        val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

         //设置exe返回
        io.core.exe(w).iresp.bits.uop  := ldq(ldq_idx).bits.uop
        io.core.exe(w).fresp.bits.uop  := ldq(ldq_idx).bits.uop
        io.core.exe(w).iresp.valid     := send_iresp
        io.core.exe(w).iresp.bits.data := io.dmem.resp(w).bits.data
        io.core.exe(w).fresp.valid     := send_fresp
        io.core.exe(w).fresp.bits.data := io.dmem.resp(w).bits.data


        /*when(io.dmem.resp(w).bits.uop.debug_pc(31,0) === 0x80001310L.U){
           printf(p" cycles=${io.core.idle_cycles} ")
           printf(p" ldq_idx=${io.dmem.resp(w).bits.uop.ldq_idx} ")
           printf(p" rob_idx=${io.dmem.resp(w).bits.uop.rob_idx} ")
           printf(p" pdst=${io.dmem.resp(w).bits.uop.pdst} ")
           printf(p" data=${io.dmem.resp(w).bits.data} ")
           printf(p"find lsu dmem-resp 80001310\n")
        }*/
        when(io.dmem.resp(w).bits.uop.debug_pc(31,0) === 0x00082022L.U){
           /*printf(p" cycles=${io.core.idle_cycles} ")
           printf(p" ldq_idx=${io.dmem.resp(w).bits.uop.ldq_idx} ")
           printf(p" rob_idx=${io.dmem.resp(w).bits.uop.rob_idx} ")
           printf(p" pdst=${io.dmem.resp(w).bits.uop.pdst} ")
           printf(p" data=${io.dmem.resp(w).bits.data} ")
           printf(p"succeeded find lsu dmem-resp 00082022\n")*/
        }

        assert(send_iresp ^ send_fresp)
        dmem_resp_fired(w) := true.B       //dmem阶段返回 发出

        ldq(ldq_idx).bits.succeeded      := io.core.exe(w).iresp.valid || io.core.exe(w).fresp.valid     //ldq执行成功
        ldq(ldq_idx).bits.debug_wb_data  := io.dmem.resp(w).bits.data      //debug_写回数据
      }

        .elsewhen (io.dmem.resp(w).bits.uop.uses_stq)       //使用stq
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        stq(io.dmem.resp(w).bits.uop.stq_idx).bits.succeeded := true.B        //stq执行成功
        when (io.dmem.resp(w).bits.uop.is_amo) {   //当是一个amo操作时，需要设置exe返回？
          dmem_resp_fired(w) := true.B
          io.core.exe(w).iresp.valid     := true.B
          io.core.exe(w).iresp.bits.uop  := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop
          io.core.exe(w).iresp.bits.data := io.dmem.resp(w).bits.data

          stq(io.dmem.resp(w).bits.uop.stq_idx).bits.debug_wb_data := io.dmem.resp(w).bits.data
        }
      }
    }


    when (dmem_resp_fired(w) && wb_forward_valid(w))      //dmem返回发出 && 写回转发有效
    {
      // Twiddle thumbs. Can't forward because dcache response takes precedence    //转动拇指。不能转发，因为dcache响应优先
       when(ldq(wb_forward_ldq_idx(w)).bits.uop.debug_pc(31,0) === 0x00082022L.U){
        //printf(p" cycles=${io.core.idle_cycles} ")
        //printf(p" rob_idx=${ldq(wb_forward_ldq_idx(w)).bits.uop.rob_idx} ")
        //printf(p" dmem_resp_fired ")
        //printf(p" dmem_resp_fired(w) && wb_forward_valid(w) 00082022\n")
      }
    }
      .elsewhen (!dmem_resp_fired(w) && wb_forward_valid(w))  //dmem返回不发出 && 写回转发有效，进行转发后的返回赋值
    {
      val f_idx       = wb_forward_ldq_idx(w)
      val forward_uop = ldq(f_idx).bits.uop
      val stq_e       = stq(wb_forward_stq_idx(w))          //load所依赖的最年轻的store：  wb_forward_stq_idx
      val data_ready  = stq_e.bits.data.valid
      val live        = !IsKilledByBranch(io.core.brupdate, forward_uop)
      val storegen = new freechips.rocketchip.rocket.StoreGen(
                                stq_e.bits.uop.mem_size, stq_e.bits.addr.bits,
                                stq_e.bits.data.bits, coreDataBytes)          //初步估计是dcache访问失败后，去访问内存
      val loadgen  = new freechips.rocketchip.rocket.LoadGen(
                                forward_uop.mem_size, forward_uop.mem_signed,
                                wb_forward_ld_addr(w),
                                storegen.data, false.B, coreDataBytes)



      /*when(ldq(wb_forward_ldq_idx(w)).bits.uop.debug_pc(31,0) === 0x80001310L.U){
        printf(p" cycles=${io.core.idle_cycles} ")
        printf(p" rob_idx=${ldq(f_idx).bits.uop.rob_idx} ")
        printf(p" find lsu storegen=${storegen.data} ")
        printf(p" find lsu loadgen=${loadgen.data} 80001310\n")
      }*/
      when(ldq(wb_forward_ldq_idx(w)).bits.uop.debug_pc(31,0) === 0x00082022L.U){
        //printf(p" cycles=${io.core.idle_cycles} ")
        //printf(p" rob_idx=${ldq(f_idx).bits.uop.rob_idx} ")
        //printf(p" find lsu storegen=${storegen.data} ")
        //printf(p" find lsu loadgen=${loadgen.data} 00082022\n")
      }

      io.core.exe(w).iresp.valid := (forward_uop.dst_rtype === RT_FIX) && data_ready && live
      io.core.exe(w).fresp.valid := (forward_uop.dst_rtype === RT_FLT) && data_ready && live
      io.core.exe(w).iresp.bits.uop  := forward_uop
      io.core.exe(w).fresp.bits.uop  := forward_uop
      io.core.exe(w).iresp.bits.data := loadgen.data
      io.core.exe(w).fresp.bits.data := loadgen.data



      when (data_ready && live) {
        ldq(f_idx).bits.succeeded := data_ready
        ldq(f_idx).bits.forward_std_val := true.B
        ldq(f_idx).bits.forward_stq_idx := wb_forward_stq_idx(w)

        ldq(f_idx).bits.debug_wb_data   := loadgen.data
      }
    }
  }

  // Initially assume the speculative load wakeup failed   //最初假定投机性load唤醒失败
  io.core.ld_miss         := RegNext(io.core.spec_ld_wakeup.map(_.valid).reduce(_||_))
  //为何是!（_.valid）
  val spec_ld_succeed = widthMap(w =>
    !RegNext(io.core.spec_ld_wakeup(w).valid) ||
    (io.core.exe(w).iresp.valid &&
      io.core.exe(w).iresp.bits.uop.ldq_idx === RegNext(mem_incoming_uop(w).ldq_idx)
    )
  ).reduce(_&&_)
  when (spec_ld_succeed) {
    io.core.ld_miss := false.B
  }


  //-------------------------------------------------------------
  // Kill speculated entries on branch mispredict         //杀死推测条目,在分支mispredict
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Kill stores                        //杀了store
  val st_brkilled_mask = Wire(Vec(numStqEntries, Bool()))
  for (i <- 0 until numStqEntries)
  {
    st_brkilled_mask(i) := false.B

    when (stq(i).valid)
    {
      stq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, stq(i).bits.uop.br_mask)

      when (IsKilledByBranch(io.core.brupdate, stq(i).bits.uop))
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B    //saq
        stq(i).bits.data.valid := false.B    //sdq
        st_brkilled_mask(i)    := true.B
      }
    }

    assert (!(IsKilledByBranch(io.core.brupdate, stq(i).bits.uop) && stq(i).valid && stq(i).bits.committed),
      "Branch is trying to clear a committed store.")
  }

  // Kill loads                       //杀死load
  for (i <- 0 until numLdqEntries)
  {
    when (ldq(i).valid)
    {
      ldq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, ldq(i).bits.uop.br_mask)
      when (IsKilledByBranch(io.core.brupdate, ldq(i).bits.uop))
      {
        ldq(i).valid           := false.B
        ldq(i).bits.addr.valid := false.B        //ldq
      }
    }
  }

  //-------------------------------------------------------------
  when (io.core.brupdate.b2.mispredict && !io.core.exception)         //分支错误预测时，设置stq/ldq——tail,  stq-tail作用是？
  {
    stq_tail := io.core.brupdate.b2.uop.stq_idx
    ldq_tail := io.core.brupdate.b2.uop.ldq_idx
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // dequeue old entries on commit                在提交时退出旧条目
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var temp_stq_commit_head = stq_commit_head
  var temp_ldq_head        = ldq_head
  for (w <- 0 until coreWidth)
  {
    val commit_store = io.core.commit.valids(w) && io.core.commit.uops(w).uses_stq
    val commit_load  = io.core.commit.valids(w) && io.core.commit.uops(w).uses_ldq
    val idx = Mux(commit_store, temp_stq_commit_head, temp_ldq_head)
    when (commit_store)         //store提交
    {
      stq(idx).bits.committed := true.B           //设置stq的提交位
    } .elsewhen (commit_load) {            //load提交
      assert (ldq(idx).valid, "[lsu] trying to commit an un-allocated load entry.")
      assert ((ldq(idx).bits.executed || ldq(idx).bits.forward_std_val) && ldq(idx).bits.succeeded ,
        "[lsu] trying to commit an un-executed load entry.")
        

      //load提交之后，将ldq条目对应位置空
      ldq(idx).valid                 := false.B
      ldq(idx).bits.addr.valid       := false.B
      ldq(idx).bits.executed         := false.B
      ldq(idx).bits.succeeded        := false.B
      ldq(idx).bits.order_fail       := false.B
      ldq(idx).bits.forward_std_val  := false.B
      ldq(idx).bits.preorder_fail    := 9.U

    }

    if (MEMTRACE_PRINTF) {          //将内存访问跟踪转储到L1D以进行调试
      when (commit_store || commit_load) {
        val uop    = Mux(commit_store, stq(idx).bits.uop, ldq(idx).bits.uop)
        val addr   = Mux(commit_store, stq(idx).bits.addr.bits, ldq(idx).bits.addr.bits)
        val stdata = Mux(commit_store, stq(idx).bits.data.bits, 0.U)
        val wbdata = Mux(commit_store, stq(idx).bits.debug_wb_data, ldq(idx).bits.debug_wb_data)
        printf("MT %x %x %x %x %x %x %x\n",
          io.core.tsc_reg, uop.uopc, uop.mem_cmd, uop.mem_size, addr, stdata, wbdata)
      }
    }

    temp_stq_commit_head = Mux(commit_store,
                               WrapInc(temp_stq_commit_head, numStqEntries),
                               temp_stq_commit_head)     //提交之后，提交头加一

    temp_ldq_head        = Mux(commit_load,
                               WrapInc(temp_ldq_head, numLdqEntries),
                               temp_ldq_head)           //提交之后，提交头加一
  }
  stq_commit_head := temp_stq_commit_head
  ldq_head        := temp_ldq_head

  // store has been committed AND successfully sent data to memory             store已提交并成功将数据发送到内存
  when (stq(stq_head).valid && stq(stq_head).bits.committed)
  {
    when (stq(stq_head).bits.uop.is_fence && !io.dmem.ordered) {     //fence指令需要顺序
      io.dmem.force_order := true.B
      store_needs_order   := true.B
    }
    clear_store := Mux(stq(stq_head).bits.uop.is_fence, io.dmem.ordered,
                                                        stq(stq_head).bits.succeeded) // fence指令排序后，clear;  store stq_head成功后可以clear
  }

  when (clear_store)
  {
    //store提交之后，将stq条目对应位置空
    stq(stq_head).valid           := false.B
    stq(stq_head).bits.addr.valid := false.B
    stq(stq_head).bits.data.valid := false.B
    stq(stq_head).bits.succeeded  := false.B
    stq(stq_head).bits.committed  := false.B
    stq(stq_head).bits.preorder_fail := 9.U

    stq_head := WrapInc(stq_head, numStqEntries)              //stq_head递增
    when (stq(stq_head).bits.uop.is_fence)
    {
      stq_execute_head := WrapInc(stq_execute_head, numStqEntries)
    }
  }


  // -----------------------
  // Hellacache interface
  // We need to time things like a HellaCache would
  // Hellacache接口
  //我们需要像HellaCache一样安排时间
  io.hellacache.req.ready := false.B
  io.hellacache.s2_nack   := false.B
  io.hellacache.s2_xcpt   := (0.U).asTypeOf(new rocket.HellaCacheExceptions)
  io.hellacache.resp.valid := false.B
  when (hella_state === h_ready) {
    io.hellacache.req.ready := true.B
    when (io.hellacache.req.fire()) {
      hella_req   := io.hellacache.req.bits
      hella_state := h_s1
    }
  } .elsewhen (hella_state === h_s1) {
    can_fire_hella_incoming(memWidth-1) := true.B

    hella_data := io.hellacache.s1_data
    hella_xcpt := dtlb.io.resp(memWidth-1)

    when (io.hellacache.s1_kill) {
      when (will_fire_hella_incoming(memWidth-1) && dmem_req_fire(memWidth-1)) {
        hella_state := h_dead
      } .otherwise {
        hella_state := h_ready
      }
    } .elsewhen (will_fire_hella_incoming(memWidth-1) && dmem_req_fire(memWidth-1)) {
      hella_state := h_s2
    } .otherwise {
      hella_state := h_s2_nack
    }
  } .elsewhen (hella_state === h_s2_nack) {
    io.hellacache.s2_nack := true.B
    hella_state := h_ready
  } .elsewhen (hella_state === h_s2) {
    io.hellacache.s2_xcpt := hella_xcpt
    when (io.hellacache.s2_kill || hella_xcpt.asUInt =/= 0.U) {
      hella_state := h_dead
    } .otherwise {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_wait) {
    for (w <- 0 until memWidth) {
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := io.dmem.resp(w).bits.data
      } .elsewhen (io.dmem.nack(w).valid && io.dmem.nack(w).bits.is_hella) {
        hella_state := h_replay
      }
    }
  } .elsewhen (hella_state === h_replay) {
    can_fire_hella_wakeup(memWidth-1) := true.B

    when (will_fire_hella_wakeup(memWidth-1) && dmem_req_fire(memWidth-1)) {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_dead) {
    for (w <- 0 until memWidth) {
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
        hella_state := h_ready
      }
    }
  }

  //-------------------------------------------------------------
  // Exception / Reset

  // for the live_store_mask, need to kill stores that haven't been committed
  //异常/重置

  //对于live_store_mask，需要杀死尚未提交的存储
  val st_exc_killed_mask = WireInit(VecInit((0 until numStqEntries).map(x=>false.B)))

  when (reset.asBool || io.core.exception)
  {
    ldq_head := 0.U
    ldq_tail := 0.U

    when (reset.asBool) //reset
    {
      stq_head := 0.U
      stq_tail := 0.U
      stq_commit_head  := 0.U
      stq_execute_head := 0.U

      for (i <- 0 until numStqEntries)
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
        stq(i).bits.uop        := NullMicroOp
      }
    }
      .otherwise // exception
    {
      stq_tail := stq_commit_head

      for (i <- 0 until numStqEntries)
      {
        when (!stq(i).bits.committed && !stq(i).bits.succeeded)
        {
          stq(i).valid           := false.B
          stq(i).bits.addr.valid := false.B
          stq(i).bits.data.valid := false.B
          st_exc_killed_mask(i)  := true.B
        }
      }
    }

    for (i <- 0 until numLdqEntries)
    {
      ldq(i).valid           := false.B
      ldq(i).bits.addr.valid := false.B
      ldq(i).bits.executed   := false.B
    }
  }

  //-------------------------------------------------------------
  // Live Store Mask
  // track a bit-array of stores that are alive
  // (could maybe be re-produced from the stq_head/stq_tail, but need to know include spec_killed entries)
  //活着的store mask
  //跟踪活着的store的位数组
  //(可以从stq_head/stq_tail重新生成，但是需要知道包含spec_killed条目)

  // TODO is this the most efficient way to compute the live store mask?
  live_store_mask := next_live_store_mask &
                    ~(st_brkilled_mask.asUInt) &
                    ~(st_exc_killed_mask.asUInt)


}

/**
 * Object to take an address and generate an 8-bit mask of which bytes within a
 * double-word. 获取一个地址并生成一个8位掩码，该掩码包含双字中的字节。
 */
object GenByteMask
{
   def apply(addr: UInt, size: UInt): UInt =
   {
      val mask = Wire(UInt(8.W))
      mask := MuxCase(255.U(8.W), Array(
                   (size === 0.U) -> (1.U(8.W) << addr(2,0)),
                   (size === 1.U) -> (3.U(8.W) << (addr(2,1) << 1.U)),
                   (size === 2.U) -> Mux(addr(2), 240.U(8.W), 15.U(8.W)),
                   (size === 3.U) -> 255.U(8.W)))
      mask
   }
}

/**
 * 找到 load 依赖的最年轻的 store
 */
class ForwardingAgeLogic(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val addr_matches    = Input(UInt(num_entries.W)) // bit vector of addresses that match
                                                       // between the load and the SAQ
      val youngest_st_idx = Input(UInt(stqAddrSz.W)) // needed to get "age"

      val forwarding_val  = Output(Bool())
      val forwarding_idx  = Output(UInt(stqAddrSz.W))
   })

   // generating mask that zeroes out anything younger than tail
   val age_mask = Wire(Vec(num_entries, Bool()))
   for (i <- 0 until num_entries)
   {
      age_mask(i) := true.B
      when (i.U >= io.youngest_st_idx) // currently the tail points PAST last store, so use >=
      {
         age_mask(i) := false.B
      }
   }

   // Priority encoder with moving tail: double length
   val matches = Wire(UInt((2*num_entries).W))
   matches := Cat(io.addr_matches & age_mask.asUInt,
                  io.addr_matches)

   val found_match = Wire(Bool())
   found_match       := false.B
   io.forwarding_idx := 0.U

   // look for youngest, approach from the oldest side, let the last one found stick
   for (i <- 0 until (2*num_entries))
   {
      when (matches(i))
      {
         found_match := true.B
         io.forwarding_idx := (i % num_entries).U
      }
   }

   io.forwarding_val := found_match
}
