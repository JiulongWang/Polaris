package nutcore

import chisel3._
import chisel3.util._

import utils._

class NeuronIO(len: Int = 16) extends NutCoreBundle{
    val in = Input(Vec(XLEN/len, UInt(len.W)))
    val weight = Input(Vec(XLEN/len, UInt(len.W)))
    val vr = Input(UInt(XLEN.W))
    val hasTs = Input(Bool())
    val en = Input(Bool())
    val tau = Input(UInt(XLEN.W))
    val res = Output(Vec(XLEN/len, UInt(len.W)))
}

class LNU_IO extends NutCoreBundle{
    val in = Flipped(Decoupled(new Bundle{
        val SCtrl = new SCtrlIO
    }))

    val out = Decoupled(new Bundle{
        val res = Output(UInt(XLEN.W))
    })
}

// class BoothMultiplier(n: Int) extends NutCoreModule {
//   val io = IO(new Bundle {
//     val en = Input(Bool())
//     val Q = Input(UInt(n.W))
//     val M = Input(UInt(n.W))
//     val res = Output(UInt((n * 2).W))
//     val valid = Output(Bool())
//   })  
//     val regQ = RegInit(0.U(n.W))
//     val regM = RegInit(0.U(n.W))
//     val regNM = RegInit(0.U(n.W))
//     val regP = RegInit(0.U((2*n + 1).W))

//     val  s_idle :: s_load :: s_compute :: s_out :: Nil = Enum(4)

//     val state = RegInit(s_idle)
//     val (cnt, wrap) = Counter(state === s_compute, n + 1)
    

    
//         switch(state){
//             is(s_idle){
//                 when((regQ =/= io.Q || regM =/= io.M) && io.en){
//                     state := s_load
//                 }
//             }
//             is(s_load){
//                 regQ := io.Q
//                 regM := io.M          
//                 regNM := ~io.M + 1.U
//                 regP := Cat(Fill(n,0.U), io.Q, 0.U)
//                 state := s_compute
//             }

//             is(s_compute){
//                 val p = WireInit(0.U((2*n + 1).W))
//                 when(regP(1) === 0.U && regP(0) === 1.U){
//                     p := regP + (regM << (n + 1))
//                 }.elsewhen(regP(1) === 1.U && regP(0) === 0.U){
//                     p := regP + (regNM << (n + 1))
//                 }.otherwise{
//                     p := regP
//                 }

//                 regP := Mux(p(2*n), Cat(1.U, p(2*n, 1)), p >> 1)

//                 when(cnt === (n-1).U){
//                     state := s_out
//                 }.otherwise{
//                     state := state
//                 }
//             }
//             is(s_out){
//                 state := s_idle
//             }
//         }
//         io.valid := state === s_out
//         io.res := regP(2*n, 1)
// }

class Neuron(len: Int = 16) extends NutCoreModule{
    val io = IO(new NeuronIO(len))

    val nu_res = WireInit(VecInit(Seq.fill(XLEN/len)(0.U(len.W))))
    val nu = WireInit(VecInit(Seq.fill(XLEN/len)(0.U(len.W))))
    val tmp_nu = WireInit(VecInit(Seq.fill(XLEN/len)(0.U((2*len).W))))
    def fix_point_cut8(x: UInt): UInt = {
        x(11, 4)
    }
    def fix_point_cut16(x: UInt): UInt = {
        x(23, 8)
    }

    when(io.en){
        for(i <- 0 until (XLEN/len)){
            when(io.hasTs){
                nu(i) := io.in(i)(7, 0)
                
                tmp_nu(i) := nu(i) + io.weight(i)
                nu_res(i) := tmp_nu(i) | (io.in(i)(15, 8) << (len/2).U)
                
                io.res(i) := nu_res(i)
                Debug("[SNN_LNU]ts(%d) = %x, nu(%d) = %x weight = %x res(%d) = %x out(%d) = %x\n",i.U, io.in(i)(15, 8), i.U, nu(i), io.weight(i), i.U, nu_res(i), i.U, io.res(i))
            }.otherwise{
                nu(i) := io.in(i)
                tmp_nu(i) := (io.vr - nu(i) + io.weight(i)) * io.tau
                nu_res(i) := nu(i) + (fix_point_cut16(tmp_nu(i)))
                io.res(i) := nu_res(i)
                Debug("[SNN_LNU]tmp_nu(%d) %x \n", i.U, tmp_nu(i))
            }
        }
    }.otherwise{
        for(i <- 0 until (XLEN/len)){
            io.res(i) := 0.U
        }
    }   
}

class LNU extends NutCoreModule{
    val io = IO(new LNU_IO)


    val nu0 = Module(new Neuron) 
    // val nu1 = Module(new Neuron) 
    // val nu2 = Module(new Neuron) 
    // val nu3 = Module(new Neuron) 

    nu0.io.en := io.in.bits.SCtrl.isNup
    nu0.io.in := io.in.bits.SCtrl.DIn1
    // nu1.io.in := io.in.bits.SCtrl.DIn1(1)
    // nu2.io.in := io.in.bits.SCtrl.DIn1(2)
    // nu3.io.in := io.in.bits.SCtrl.DIn1(3)
    nu0.io.weight := io.in.bits.SCtrl.DIn2
    // nu1.io.weight := io.in.bits.SCtrl.DIn2(1)
    // nu2.io.weight := io.in.bits.SCtrl.DIn2(2)
    // nu3.io.weight := io.in.bits.SCtrl.DIn2(3)
    nu0.io.hasTs := io.in.bits.SCtrl.hasTs
    // nu1.io.hasTs := io.in.bits.SCtrl.hasTs
    // nu2.io.hasTs := io.in.bits.SCtrl.hasTs
    // nu3.io.hasTs := io.in.bits.SCtrl.hasTs
    nu0.io.vr := io.in.bits.SCtrl.SRF4(SRFAddr.VR)
    // nu1.io.vr := io.in.bits.SCtrl.SRF4(SRFAddr.VR)
    // nu2.io.vr := io.in.bits.SCtrl.SRF4(SRFAddr.VR)
    // nu3.io.vr := io.in.bits.SCtrl.SRF4(SRFAddr.VR)
    nu0.io.tau := io.in.bits.SCtrl.SRF4(SRFAddr.TAU)
    // nu1.io.tau := io.in.bits.SCtrl.SRF4(SRFAddr.TAU)
    // nu2.io.tau := io.in.bits.SCtrl.SRF4(SRFAddr.TAU)
    // nu3.io.tau := io.in.bits.SCtrl.SRF4(SRFAddr.TAU)

    io.out.bits.res := Mux(io.in.bits.SCtrl.isNup, nu0.io.res.reverse.reduce(Cat(_,_)), 0.U)

    io.in.ready := !io.in.valid
    io.out.valid := io.in.valid

    Debug(io.in.bits.SCtrl.isNup, "[SNN_LNU] hasTs: %b, n0: %x n1: %x n2: %x n3: %x\n", io.in.bits.SCtrl.hasTs, io.in.bits.SCtrl.DIn1(0), io.in.bits.SCtrl.DIn1(1), io.in.bits.SCtrl.DIn1(2), io.in.bits.SCtrl.DIn1(3))
}