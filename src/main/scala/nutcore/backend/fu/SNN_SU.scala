package nutcore

import chisel3._
import chisel3.util._

import utils._

class SU_IO extends NutCoreBundle{
    val in = Flipped(Decoupled(new Bundle{
        val SCtrl = new SCtrlIO
    }))

    val out = Decoupled(new Bundle{
        val res = Output(UInt(XLEN.W))
        // val acc = Output(UInt(XLEN.W))
    })
}

// class SUMIO extends NutCoreBundle{
//     val in = Input(Vec(4, UInt(16.W)))
//     val masks = Input(Vec(4, UInt(16.W)))
//     val acc = Input(UInt(64.W))
//     val isSpop = Input(Bool())
//     val isSum = Input(Bool())
//     val hasAcc = Input(Bool())
//     val res = Output(UInt(64.W))
// }



class CalcQ extends NutCoreModule{
    val io = IO(new Bundle{
        val en = Input(Bool())
        val din = Input(UInt(16.W))
        val Q = Output(UInt(16.W))
        val Y = Output(UInt(16.W))
        val valid = Output(Bool())
    })

    val paraQ = 5909.U
    val paraY = 2839.U
    val tmpQ = RegInit(0.U(32.W))
    val tmpY = RegInit(0.U(32.W))
    // val mulqEn = RegInit(false.B)
    // val mulyEn = RegInit(false.B)

    val s_idle :: s_calcQ :: s_calcY :: s_out :: Nil = Enum(4)
    val state = RegInit(s_idle)
       
    switch(state){
        is(s_idle){
          when(io.en){
            state := s_calcQ
            // mulqEn := true.B
          }.otherwise{
            state := state
            // mulqEn := false.B
          }

            
        }
        is(s_calcQ){
            //  val mulq = Module(new BoothMultiplier(16))
            //   mulq.io.en := mulqEn
            //   mulq.io.Q := io.din
            //   mulq.io.M := paraQ
 
            // mulyEn := Mux(mulq.io.valid, true.B, false.B)
            state := s_calcY//Mux(mulq.io.valid, s_calcY, s_calcQ)
            // mulqEn := Mux(mulq.io.valid, false.B, true.B)
            tmpQ := io.din * paraQ// Mux(mulq.io.valid, mulq.io.res,0.U)
        }
        is(s_calcY){
            //  val muly = Module(new BoothMultiplier(16))
            //   muly.io.en := mulyEn
            //   muly.io.Q := tmpQ(23, 12)
            //   muly.io.M := paraY
 
            // mulyEn := Mux(muly.io.valid, false.B, true.B)
            state := s_out// Mux(muly.io.valid, s_out, s_calcY)         
            tmpY := tmpQ(23, 12) * paraY// Mux(muly.io.valid, muly.io.res,0.U)
        }
        is(s_out){
            state := Mux(io.en, state, s_idle)
        }
    }

    
   
    val Q = Cat(Fill(12, 0.U), tmpQ(27, 24))
    io.Q := Q 

    val Y = Mux(tmpY(31), Cat(Fill(4, 1.U), tmpY(27, 12)), tmpY >> 12)
    io.Y := Y

    io.valid := state === s_out
}

class EXP extends NutCoreModule{
  val io = IO(new Bundle{
    val en = Input(Bool())
    val din = Input(SInt(16.W))
    val dout = Output(SInt(16.W))
    val valid = Output(Bool())
  })

  val calcQ = Module(new CalcQ)
  calcQ.io.din := io.din.asUInt
  calcQ.io.en := io.en
  val Q = RegInit(0.U(16.W))
  val Y = RegInit(0.U(16.W))
  Q := Mux(calcQ.io.valid, calcQ.io.Q, Q)
  Y := Mux(calcQ.io.valid, calcQ.io.Y, Y)

  val An = VecInit(Array(
    ("b0000100011001010".U).zext,
    ("b0000010000010110".U).zext,
    ("b0000001000000011".U).zext,
    ("b0000000100000000".U).zext,
    ("b0000000100000000".U).zext,
    ("b0000000010000000".U).zext,
    ("b0000000001000000".U).zext,
    ("b0000000000100000".U).zext,
    ("b0000000000010000".U).zext,
    ("b0000000000001000".U).zext,
    ("b0000000000000100".U).zext,
    ("b0000000000000010".U).zext
    
  ))

  val y = RegInit(VecInit(Seq.fill(13)(0.S(16.W))))
  val z = RegInit(VecInit(Seq.fill(13)(0.S(16.W))))
  val x = RegInit(VecInit(Seq.fill(13)(0.S(16.W))))
  val valid = RegInit(VecInit(Seq.fill(13)(false.B)))
  valid(0) := Mux(valid.reduce( _ || _), false.B, calcQ.io.valid)

//   val (cnt, wrap) = Counter(~QEn && io.en, 7)

  for(i <- 0 to 12){
    when(valid(12)){
      x(i) := 0.S
      y(i) := 0.S
      z(i) := 0.S
      valid(i) := false.B
    }.elsewhen(calcQ.io.valid && io.en){
      if(i == 0){
        x(i) := ("b0001001101000111".U).zext
        y(i) := 0.S
        z(i) := Mux(Y(15), Y.asSInt, Y.zext)
      }else if(i % 2 == 1){
        val tmp_x = Mux(z(i-1)(15), x(i-1) - (y(i-1) >> i), x(i-1) + (y(i-1) >> i))
        val tmp_y = Mux(z(i-1)(15), y(i-1) - (x(i-1) >> i), y(i-1) + (x(i-1) >> i))
        val tmp_z = Mux(z(i-1)(15), z(i-1) + An(i-1), z(i-1) - An(i-1))
        val tmp_valid = valid(i - 1)
        x(i) := tmp_x
        y(i) := tmp_y
        z(i) := tmp_z
        valid(i) := tmp_valid
        x(i+1) := Mux(tmp_z(15), tmp_x - (tmp_y >> (i + 1)), tmp_x + (tmp_y >> (i + 1)))
        y(i+1) := Mux(tmp_z(15), tmp_y - (tmp_x >> (i + 1)), tmp_y + (tmp_x >> (i + 1)))
        z(i+1) := Mux(tmp_z(15), tmp_z + An(i), tmp_z - An(i))
        valid(i + 1) := valid(i)
      }
    }.otherwise{
      x(i) := 0.S
      y(i) := 0.S
      z(i) := 0.S
      valid(i) := false.B
    }
  }  
  val res = x(12) + y(12)

  io.valid := valid(12)
  io.dout := res << Q

  // for (i <- 0 until 13){
  //   Debug("[SNN exp] din: %x x(%d) %x y(%d) %x z(%d) %x Q %x Y %x valid(%d) %x\n",io.din.asUInt, i.U, x(i), i.U, y(i), i.U, z(i), Q, Y, i.U, valid(i))
  // }
}

// class SUM extends NutCoreModule{
//     val io = IO(new SUMIO)
  
//     val valids = WireInit(VecInit(Seq.fill(4)(0.U(16.W))))
//     for(i <- 0 until 4){
//         valids(i) := Mux(io.masks(i) === 1.U, io.in(i), 0.U)
//     }

//     val tot = valids.reduce(_ + _)

//     when(io.isSpop && io.hasAcc){
//         io.res := tot + io.acc
//     }.elsewhen(io.isSpop && !io.hasAcc || io.isSum){
//         io.res := tot
//     }.otherwise{
//         io.res := 0.U
//     }

// }

class SUForward extends NutCoreModule{
    val io = IO(new Bundle{
        val in_valid = Input(Bool())
        val src1 = Input(Vec(4, UInt(16.W)))
        val src2 = Input(Vec(4, UInt(16.W)))
        val isTdr = Input(Bool())
        val isExp = Input(Bool())
        val res = Output(Vec(4, UInt(16.W)))
        val valid = Output(Bool())
    })

    val ts0 = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
    val ts1 = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
    val ts_res = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
    val res = RegInit(VecInit(Seq.fill(4)(0.S(16.W))))
    val valid = RegInit(VecInit(Seq.fill(2)(false.B)))
    valid(0) := Mux(valid.reduce(_ || _), false.B, io.in_valid && io.isTdr)



    when(io.in_valid &&io.isTdr){
        for(i <- 0 until 4){
            ts0(i) := (io.src1(i)(15, 8))
            ts1(i) := (io.src2(i)(15, 8))
            ts_res(i) := ts0(i) - ts1(i)
        }
        io.valid := RegNext(RegNext(io.in_valid))
    }.elsewhen(io.in_valid &&io.isExp){
        val exp0 = Module(new EXP)
        val exp1 = Module(new EXP)
        val exp2 = Module(new EXP)
        val exp3 = Module(new EXP)
        exp0.io.din := Mux(io.src1(0)(15), io.src1(0).asSInt, io.src1(0).zext)
        exp1.io.din := Mux(io.src1(1)(15), io.src1(1).asSInt, io.src1(1).zext)
        exp2.io.din := Mux(io.src1(2)(15), io.src1(2).asSInt, io.src1(2).zext)
        exp3.io.din := Mux(io.src1(3)(15), io.src1(3).asSInt, io.src1(3).zext)
        exp0.io.en := io.in_valid // && io.src2(0) === 1.U 
        exp1.io.en := io.in_valid // && io.src2(1) === 1.U
        exp2.io.en := io.in_valid // && io.src2(2) === 1.U
        exp3.io.en := io.in_valid // && io.src2(3) === 1.U
        res(0) := Mux(exp3.io.valid, exp3.io.dout, res(0))
        res(1) := Mux(exp2.io.valid, exp2.io.dout, res(1))
        res(2) := Mux(exp1.io.valid, exp1.io.dout, res(2))
        res(3) := Mux(exp0.io.valid, exp0.io.dout, res(3))
        io.valid := RegNext(exp0.io.valid && exp1.io.valid && exp2.io.valid && exp3.io.valid)
    }.otherwise{
        io.valid := false.B
    }

    for(i <- 0 until 4){
        io.res(i) := Mux(io.isExp, res(i).asUInt, Mux(ts_res(i)(7), SignExt(ts_res(i), 16), ZeroExt(ts_res(i), 16)))
        Debug(io.in_valid &&io.isTdr, "[SNN_SU_SUF] ts0 %x ts1 %x res %x\n", ts0(i), ts1(i), io.res(i))
    }
    
}

class SU(len: Int = 16) extends NutCoreModule{
    val io = IO(new SU_IO)

    val (src1, src2, acc) = (io.in.bits.SCtrl.DIn1, io.in.bits.SCtrl.DIn2, io.in.bits.SCtrl.SRF4(SRFAddr.ACC))

    val suf = Module(new SUForward)
    // val sum = Module(new SUM)
    suf.io.src1 := src1
    suf.io.src2 := src2
    suf.io.isTdr := io.in.bits.SCtrl.isTdr
    suf.io.isExp := io.in.bits.SCtrl.isExp
    suf.io.in_valid := io.in.valid

    
    // sum.io.in := src1
    // sum.io.masks := src2
    // sum.io.isSpop := io.in.bits.SCtrl.isSpop
    // sum.io.acc := acc
    // sum.io.hasAcc := io.in.bits.SCtrl.hasAcc
    // sum.io.isSum := io.in.bits.SCtrl.isSum

    val res = WireInit(0.U(XLEN.W))
    val bp_valid = WireInit(false.B)

    when(io.in.bits.SCtrl.isBpo){
      val sp03 = src1
      val target = src2
      var l = List(0.U)

      for(i <- 0 until XLEN/len){
        val tmp = Mux(target(i) === 1.U, 
                    Mux(sp03(i) === 1.U, 0.U, 1.U), 
                    Mux(sp03(i) === 1.U, Fill(len , 1.U), 0.U))
        l = tmp :: l  
      }
     res := l.dropRight(1).reduce(Cat(_, _))
     io.out.bits.res := res 
     bp_valid := io.in.valid
    // }.elsewhen(io.in.bits.SCtrl.isBpm || io.in.bits.SCtrl.isSpop && io.in.bits.SCtrl.hasAcc){
    //   val ksiw03 = RegInit(VecInit(Seq.fill(4)(0.U(16.W))))

    //   for(i <- 0 until XLEN/len){
    //     ksiw03(i) := Mux(src2(i) === 1.U, src1(i), 0.U(16.W))
    //   }

    //   res := ksiw03.reduce(_ + _) + acc
    //   bp_valid := io.in.valid
      

    // }.elsewhen(io.in.bits.SCtrl.isSum){
    //   val ksiw03 = RegInit(VecInit(Seq.fill(4)(0.U(16.W))))

    //   for(i <- 0 until XLEN/len){
    //     ksiw03(i) := Mux(src2(i) === 1.U, src1(i), 0.U(16.W))
    //   }

    //   res := Mux(io.in.bits.SCtrl.hasAcc, ksiw03.reduce(_ + _) + acc, ksiw03.reduce(_ + _))
    //   bp_valid := io.in.valid

    }.otherwise{
     res := suf.io.res.reduce(Cat(_, _))
    }
    Debug(io.in.bits.SCtrl.isTdr, "[SNN_SU] valid %b isTdr %b \n", io.in.valid, io.in.bits.SCtrl.isTdr)
    Debug("[SNN_SU] src1 %x src2 %x res %x io.res %x invalid %b outvalid %b\n", src1.reduce(Cat(_,_)), src2.reduce(Cat(_,_)), res, io.out.bits.res, io.in.valid, io.out.valid)
    io.out.bits.res := res 
    io.in.ready := io.out.ready
    io.out.valid := Mux(io.in.bits.SCtrl.isTdr || io.in.bits.SCtrl.isExp, suf.io.valid, bp_valid)
    // io.out.bits.acc := Mux(io.in.bits.SCtrl.isSum, res, acc)
}