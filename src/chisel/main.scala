// https://learn-fpag-easily.com
// Author : Theophile Loubiere

package rule90
import chisel3._
import chisel3.util._
import scala.util._

class PulseGenerator(pulsePeriod: Int) extends Module{
    val io = IO(new Bundle{
        val pulse = Output(UInt(1.W))
    })
    val pulse = RegInit(0.U(log2Ceil(pulsePeriod).W))
    pulse := Mux(pulse===pulsePeriod.U, 0.U, pulse+1.U)
    io.pulse := pulse===pulsePeriod.U
}

class cellAutomataBundle(numCells: Int) extends Bundle{
    val i_cells = Input(Vec(numCells, UInt  (1.W)))
    val o_cells = Output(Vec(numCells, UInt(1.W)))
}

class Rule110(numCells: Int) extends Module{
    val io = IO(new cellAutomataBundle(numCells))
    val biomeStates  = RegInit(VecInit(Seq.fill(numCells)(0.U(3.W))))

    for(idx <- 0 until numCells){
        val previous_idx = Math.floorMod(idx - 1, numCells)
        val next_idx     = Math.floorMod(idx + 1, numCells)
        biomeStates(idx) := io.i_cells(previous_idx) ## io.i_cells(idx) ## io.i_cells(next_idx)
        when(biomeStates(idx)===0.U || biomeStates(idx)===4.U || biomeStates(idx)===7.U){
            io.o_cells(idx):= 0.U
        }.otherwise{
            io.o_cells(idx):= 1.U
        }
    }
}

class EqualityChecker(width: Int) extends Module {
    val io = IO(new Bundle{
        val input1 = Input(UInt(width.W))
        val input2 = Input(UInt(width.W))
        val output = Output(UInt(1.W))
    })
    io.output:= io.input1 === io.input2
}

class ReduceOr(nbValue:Int) extends Module {
    val io = IO(new Bundle{
        val inputs       = Input(Vec(nbValue, UInt(1.W)))
        val output     = Output(UInt(1.W))
    })

    io.output := io.inputs.asUInt.orR
}
class OneModuleToRuleThemAll(numCells: Int) extends Module{
    val io = IO(new cellAutomataBundle(numCells))

    val biomeStates        = RegInit(VecInit(Seq.fill(numCells)(0.U(3.W))))
    val deathConditions    = List(0.U, 2.U, 5.U, 7.U) // RULE 90
    val numDeathConditions = deathConditions.length
    val judges             = List.fill(numCells*numDeathConditions)(Module(new EqualityChecker(3)))
    val verdicts           = List.fill(numCells)(Module(new ReduceOr(numDeathConditions)))

    for(idx <- 0 until numCells){
        val previous_idx = Math.floorMod(idx - 1, numCells)
        val next_idx     = Math.floorMod(idx + 1, numCells)
        biomeStates(idx) := io.i_cells(previous_idx) ## io.i_cells(idx) ## io.i_cells(next_idx)
        for(death <- 0 until numDeathConditions){
            judges(idx*numDeathConditions+death).io.input1:=biomeStates(idx)
            judges(idx*numDeathConditions+death).io.input2:=deathConditions(death)
            verdicts(idx).io.inputs(death) := judges(idx*numDeathConditions+death).io.output
        }
        io.o_cells(idx) := !verdicts(idx).io.output

    }
}

class AlchitryCUTop extends Module {
    val numCells = 24
    val io = IO(new Bundle{
        val ledPins = Output(Vec(numCells, UInt(1.W)))
    })
    // the alchitry CU board has an active low reset
    val reset_n = !reset.asBool
    val fpgafreq = 100000000

    withReset(reset_n){
        val cellular_automata = Module(new OneModuleToRuleThemAll(numCells))
        val next_generation = Module(new PulseGenerator(fpgafreq))
        for(idx <- 0 until numCells){
            cellular_automata.io.i_cells(idx) := RegEnable(cellular_automata.io.o_cells(idx), if (idx==12) 1.U else 0.U, next_generation.io.pulse.asBool)
        }
        io.ledPins <> cellular_automata.io.o_cells
    }
}

object Main extends App{
    (new chisel3.stage.ChiselStage).emitVerilog(new AlchitryCUTop, Array("--target-dir", "build/artifacts/netlist/"))
}