package nutcore

import chisel3._
import chisel3.util._

import utils._

object SNNOpType{
    def nup = "b0000".U
    def bpo = "b0001".U
    def sacc = "b0010".U
    def exp = "b0011".U
    // def spop = "b0100".U
    def tdr = "b0101".U
    def sum = "b0110".U
    def stau = "b0111".U
    def slr = "b1000".U
    def svr = "b1001".U
}

class SNNU_IO extends FunctionUnitIO{
    val dcIn = Flipped(new DecodeIO)
    val dcOut = new DecodeIO
    val FirstStageFire = Output(Bool())
    val flush = Input(Bool())
}

class SNNU extends NutCoreModule{
    val io = IO(new SNNU_IO)
    val (valid, src1, src2, func) = (io.in.valid, io.in.bits.src1, io.in.bits.src2, io.in.bits.func)
    def access(valid: Bool, src1: UInt, src2: UInt, func: UInt): UInt = {
        this.valid := valid
        this.src1 := src1
        this.src2 := src2
        this.func := func
        io.out.bits
    }
    val funct3 = io.dcIn.ctrl.funct3

    val SNNISU = Module(new SNNISU)
    val LNU = Module(new LNU)
    val SU = Module(new SU)

    def isSU(func: UInt): Bool = funct3 =/= "b000".U && funct3 =/= "b111".U && !(funct3 === "b101".U)
    def isISU(func: UInt): Bool = funct3 === "b111".U || funct3 === "b101".U

    // val isuOut = RegInit(0.U.asTypeOf(new Bundle{val SCtrl = new SCtrlIO})) 
    // val in_valid_next = RegInit(false.B)    
    // val out_valid_next = RegInit(false.B)
    // val LNU_out_bits = RegInit(0.U(XLEN.W))
    // val SU_out_bits = RegInit(0.U(XLEN.W))


    // SNNISU.io.dcIn := io.dcIn
    // in_valid_next := Mux(io.flush, false.B, valid)
    // out_valid_next := Mux(isSU(func), SU.io.out.valid, LNU.io.out.valid)

    // // LNU.io.in.bits.SCtrl := Mux(LNU.io.in.ready, isuOut.SCtrl, 0.U)
    //  when(LNU.io.in.ready || SU.io.in.ready){
    //     isuOut.SCtrl := SNNISU.io.SCtrl
    // }
    // LNU.io.in.bits.SCtrl := isuOut.SCtrl
    // LNU.io.in.valid := in_valid_next
    // LNU_out_bits := Mux(LNU.io.out.valid, LNU.io.out.bits.res, 0.U)
    // LNU.io.out.ready := io.out.ready
    
    // // SU.io.in.bits.SCtrl := Mux(SU.io.in.ready, isuOut.SCtrl, 0.U)
    // SU.io.in.bits.SCtrl := isuOut.SCtrl
    // SU.io.in.valid := in_valid_next
    // SU_out_bits := Mux(SU.io.out.valid, SU.io.out.bits.res, 0.U)
    // SU.io.out.ready := io.out.ready

    // SNNISU.io.commit_valid := SU.io.out.valid
    // SNNISU.io.AccWr := SU.io.out.bits.acc
    // // out

    // io.out.bits := Mux(isISU(func), src1, Mux(isSU(func), SU_out_bits, LNU_out_bits))
    // io.out.valid := Mux(isISU(func), valid, out_valid_next)
    // io.in.ready := Mux(isISU(func), io.out.ready, Mux(isSU(func), SU.io.in.ready, LNU.io.in.ready))
    // io.dcOut := io.dcIn
    // Debug("[SNNU]flush: %b, instr: %x, valid: %b, src1: %x, src2: %x, isISU: %b, isSU: %b\n", io.flush, io.dcIn.cf.instr, valid, src1, src2, isISU(func), isSU(func))

    val busy = RegInit(false.B)
    busy := Mux(io.in.fire(), !busy, Mux(LNU.io.in.valid || SU.io.in.valid, !busy, busy))
    // output connection
    io.in.ready := !valid || io.FirstStageFire// Mux(isISU(func), !valid, Mux(isSU(func), SU.io.in.ready, LNU.io.in.ready))
    io.out.bits := Mux(isISU(func), SNNISU.io.res, Mux(isSU(func), SU.io.out.bits.res, LNU.io.out.bits.res))
    io.dcOut := io.dcIn
    io.FirstStageFire := !busy
    LNU.io.out.ready := Mux(isSU(func), false.B, io.out.ready)
    SU.io.out.ready := Mux(isSU(func), io.out.ready, false.B)
    // io.lnuready := LNU.io.in.ready
    // io.suready := SU.io.in.ready
    io.out.valid := Mux(isISU(func), Mux(func === SNNOpType.sum, SNNISU.io.sumValid, valid), Mux(isSU(func), SU.io.out.valid, LNU.io.out.valid))
    // connect SNNISU
    SNNISU.io.dcIn := io.dcIn
    SNNISU.io.valid := valid
    // SNNISU.io.commit_valid := SU.io.out.valid
    // SNNISU.io.AccWr := SU.io.out.bits.acc
   

    // connect SNNISU and LNU
    val LNU_bits_next = Wire(new Bundle{val SCtrl = new SCtrlIO})
    val LNU_bits      = RegInit(0.U.asTypeOf(new Bundle{val SCtrl = new SCtrlIO}))
    LNU_bits_next := LNU_bits
    val LNU_valid = RegInit(0.U.asTypeOf(Bool()))
    val LNU_valid_next = Wire(Bool())
    LNU_valid_next := LNU_valid
    when(LNU.io.out.fire()){LNU_valid_next := false.B}
    when(valid && LNU.io.in.ready && !isSU(func) && !isISU(func)){
        LNU_valid_next := true.B
        LNU_bits_next.SCtrl := SNNISU.io.SCtrl
    }
    when(io.flush){LNU_valid_next := false.B}
    LNU_bits := LNU_bits_next
    LNU_valid := LNU_valid_next
    LNU.io.in.valid := LNU_valid
    LNU.io.in.bits := LNU_bits
    
    
    // connect SNNISU and SU
    val SU_bits_next = Wire(new Bundle{val SCtrl = new SCtrlIO})
    val SU_bits      = RegInit(0.U.asTypeOf(new Bundle{val SCtrl = new SCtrlIO}))
    SU_bits_next := SU_bits
    val SU_valid    = RegInit(0.U.asTypeOf(Bool()))
    val SU_valid_next = Wire(Bool())
    SU_valid_next := SU_valid
    when(SU.io.out.fire()){SU_valid_next := false.B}
    when(valid && SU.io.in.ready && isSU(func)){
        SU_valid_next := true.B
        SU_bits_next.SCtrl := SNNISU.io.SCtrl
    }
    when(io.flush){SU_valid_next := false.B}
    SU_bits := SU_bits_next
    SU_valid := SU_valid_next
    SU.io.in.bits := SU_bits
    SU.io.in.valid := SU_valid// Mux(SNNISU.io.SCtrl.isSum || SNNISU.io.SCtrl.isBpm || SNNISU.io.SCtrl.isBpo, io.in.valid, SU_valid)

    Debug("[SNNU]flush: %b, instr: %x, valid: %b, src1: %x, src2: %x, isISU: %b, isSU: %b\n", io.flush, io.dcIn.cf.instr, valid, src1, src2, isISU(func), isSU(func))
    Debug(SU.io.out.fire() || LNU.io.out.fire(), "[SNNU] out %x io.out %x valid %b\n", Mux(SU.io.out.fire() && isSU(func), SU.io.out.bits.res, LNU.io.out.bits.res), io.out.bits, io.out.valid)
    
    
}