package nutcore

import chisel3._
import chisel3.util._

import utils._

class SRF_IO extends NutCoreBundle {
    // val in = Flipped(Decoupled(new Bundle{
    //     val dcIn = new DecodeIO
    //     val SCtrl = new SCtrlIO
    // }))

    // val out = Decoupled(new Bundle{
    //     val res = Output(UInt(XLEN.W))
    //     val dcOut = new DecodeIO
    // })
    val dcIn = Flipped(new DecodeIO)
    val SCtrl = Flipped(new SCtrlIO)
    val SRF4 = Vec(4, Output(UInt(64.W)))
    val sumres = Input(UInt(64.W))
    val sumvalid = Input(Bool())
}

class SRF_NEW extends NutCoreModule {
    val io = IO(new SRF_IO)

    val srf = new SRegFile
    // val outdc = Reg(new Bundle{val dcOut = new DecodeIO; val res = Output(UInt(64.W))})

    // val outvalid = RegInit(false.B)
    // outdc.dcOut := 0.U.asTypeOf(new DecodeIO)
    // outdc.res := 0.U(64.W)
    when(io.SCtrl.isSvr){
        srf.write(SRFAddr.TAU, ZeroExt(io.SCtrl.DIn1(SRFAddr.TAU), 64))
        srf.write(SRFAddr.VR, ZeroExt(io.SCtrl.DIn1(SRFAddr.VR), 64))
        srf.write(SRFAddr.LR, ZeroExt(io.SCtrl.DIn1(SRFAddr.LR), 64))
        srf.write(SRFAddr.ACC, io.dcIn.data.src2 )
    }.elsewhen(io.SCtrl.isSum && io.SCtrl.hasAcc){
        srf.write(SRFAddr.ACC, io.sumres)
    }
    for(i <- 0 until( XLEN/ 16)){
        io.SRF4(i) := srf.read(i.U)
        Debug("[SNNISU] srf(%d) %x\n", i.U, io.SRF4(i))
    }
}

class SNNISU_NEW extends NutCoreModule{
    val io = IO(new Bundle{
        val srf4In = Vec(4, Input(UInt(64.W)))
        val dcIn = Flipped(new DecodeIO)
        val SCtrl = new SCtrlIO
        val dcOut = new DecodeIO
    })

    val src1  = io.dcIn.data.src1
    val src2  = io.dcIn.data.src2
    val func  = io.dcIn.ctrl.fuOpType

    io.dcOut := io.dcIn

    io.SCtrl.isNup  := func === SNNOpType.nup
    io.SCtrl.isBpo  := func === SNNOpType.bpo 
    io.SCtrl.isExp  := func === SNNOpType.exp 
    io.SCtrl.isTdr  := func === SNNOpType.tdr 
    io.SCtrl.isSum  := func === SNNOpType.sum 
    io.SCtrl.isSvr  := func === SNNOpType.svr 
    io.SCtrl.hasTs  := func === SNNOpType.nup && io.dcIn.cf.instr(25) === "b1".U
    io.SCtrl.hasAcc := func === SNNOpType.sum && io.dcIn.cf.instr(25) === "b1".U
    
    val len = 16
    
    for(i <- 0 until( XLEN/ len)){
        io.SCtrl.DIn1(i) := src1(len * i + len - 1, len * i)
        io.SCtrl.DIn2(i) := src2(len * i + len - 1, len * i)
        // io.SCtrl.SRF4(i) := srf.read(i.U)
        io.SCtrl.SRF4(i) := io.srf4In(i)
        Debug("DIN1(%d) %x DIN2(%d) %x SRF4(%d) %x\n", i.U, io.SCtrl.DIn1(i), i.U, io.SCtrl.DIn2(i), i.U, io.SCtrl.SRF4(i))
    }
}

class LNU_NEW extends NutCoreModule{
    val io = IO(new LNU_IO)


    val nu_res = WireInit(VecInit(Seq.fill(XLEN/16)(0.U(16.W))))
    val nu = WireInit(VecInit(Seq.fill(XLEN/16)(0.U(16.W))))
    val tmp_nu = WireInit(VecInit(Seq.fill(XLEN/16)(0.U(16.W))))
    val ksiw03 = WireInit(VecInit(Seq.fill(4)(0.U(16.W))))
    val sumres = WireInit(0.U(XLEN.W))
    when(io.in.bits.SCtrl.isNup){
        for(i <- 0 until (XLEN/16)){
            when(io.in.bits.SCtrl.hasTs){
                val tau = io.in.bits.SCtrl.SRF4(SRFAddr.TAU)
                val vr = io.in.bits.SCtrl.SRF4(SRFAddr.VR)
                nu(i) := io.in.bits.SCtrl.DIn1(i)(7, 0)
                
                tmp_nu(i) := nu(i) + ((vr + io.in.bits.SCtrl.DIn2(i)) >> tau) - (nu(i) >> tau)
                nu_res(i) := Cat(io.in.bits.SCtrl.DIn1(i)(15, 8), tmp_nu(i)) 
                Debug("[LNU] NU(%d) %x NURES(%d) %x\n", i.U, nu(i), i.U, nu_res(i))
            }.otherwise{
                val tau = io.in.bits.SCtrl.SRF4(SRFAddr.TAU)
                val vr = io.in.bits.SCtrl.SRF4(SRFAddr.VR)
                nu(i) := io.in.bits.SCtrl.DIn1(i)
                val weight = io.in.bits.SCtrl.DIn2(i)
                nu_res(i) := nu(i) + ((vr + weight) >> tau) - (nu(i) >> tau)
                Debug("[LNU] NU(%d) %x SP(%d) %x VR %x TAU %x NURES(%d) %x\n", i.U, nu(i),i.U, weight, vr, tau, i.U, nu_res(i))
            }
        }
    }.elsewhen(io.in.bits.SCtrl.isSvr){
        nu_res(0) := 1.U
    }.elsewhen(io.in.bits.SCtrl.isSum){
        val acc = io.in.bits.SCtrl.SRF4(SRFAddr.ACC)
        for(i <- 0 until XLEN/16){
          ksiw03(i) := Mux(io.in.bits.SCtrl.DIn2(i) === 1.U, io.in.bits.SCtrl.DIn1(i), 0.U(16.W))
          Debug("[SNNISU] k(%d) %x\n", i.U, ksiw03(i))
        }

        sumres := Mux(io.in.bits.SCtrl.hasAcc && RegNext(io.in.valid), ksiw03.reduce(_ + _) + acc, ksiw03.reduce(_ + _))
    } 
    io.out.bits.res := Mux(io.in.bits.SCtrl.isSum, sumres, nu_res.reverse.reduce(Cat(_,_))) 


    io.in.ready := !io.in.valid
    io.out.valid :=  io.in.valid

    io.out.bits.dcOut :=  io.in.bits.dcIn
    Debug("[LNU] READY %x OUTVALID %x INVALID %x RES %x\n", io.in.ready, io.out.valid, io.in.valid, io.out.bits.res)
}