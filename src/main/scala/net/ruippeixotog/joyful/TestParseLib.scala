package net.ruippeixotog.joyful

import java.io.File

import scala.util.Try

import net.ruippeixotog.joyful.interpreter.Interpreter
import net.ruippeixotog.joyful.lang.Parser

object TestParseLib extends App {
  new File("src/main/resources/lib").listFiles.foreach { f =>
    println(s"\n---\nParsing $f...")
    val prog = Parser.parseFile(f)
    println(s"Running $f...")
    Try(Interpreter.run(prog)).recover {
      case ex =>
        println(s"FAILED: $f (${ex.getClass.getName})")
        // println("ERROR: " + ex.getMessage)
        // ex.getStackTrace.take(5).foreach(println)
    }
  }
  println("No errors found.")
}
