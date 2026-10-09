package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        ins match {
            case PushI(f) => (f :: stack, env)
            case AddI => stack match {
                case (top :: second :: rest) => ((top + second) :: rest, env)
                case _ => throw new IllegalArgumentException("AddI failed")}
            case SubI => stack match {
                case (top :: second :: rest) => ((second - top) :: rest, env)
                case _ => throw new IllegalArgumentException("SubI failed")}
            case PopI => stack match {
                case (_ :: rest) => (rest, env)
                case _ => throw new IllegalArgumentException("PopI failed")}
            case StoreI(s) => stack match {
                case (x :: rest) => ((rest, env + (s -> x)))
                case _  => throw new IllegalArgumentException("StoreI failed")}
            case LoadI(s) =>
                if (env.contains(s)) ((env(s) :: stack), env)
                else throw new IllegalArgumentException("Value not in environment")
            case MultI => stack match {
                case (top :: second :: rest) => ((top * second) :: rest, env)
                case _ => throw new IllegalArgumentException("MultI failed")}
            case DivI => stack match {
                case (top :: second :: rest) =>
                    if (top != 0) ((second / top) :: rest, env) else throw new IllegalArgumentException ("Cannot divide by 0")
                case _ => throw new IllegalArgumentException("DivI failed")}
            case ExpI => stack match {
                case (top :: rest) => ((math.exp(top) :: rest, env))
                case _ => throw new IllegalArgumentException("ExpI failed")}
            case LogI => stack match {
                case (top :: rest) =>
                    if (top >= 0) ((math.log(top) :: rest, env)) else throw new IllegalArgumentException ("Value must be non-negative")
                case _ => throw new IllegalArgumentException("LogI failed")}
            case SinI => stack match {
                case (top :: rest) => ((math.sin(top):: rest, env))
                case _ => throw new IllegalArgumentException("SinI falied")            }
            case CosI => stack match {
                case (top :: rest) => ((math.cos(top) :: rest, env))
                case _ => throw new IllegalArgumentException("CosI failed")
            }
        }

    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
        {
            val finalEnv = {
                instructionList.foldLeft[(List[Double], Map[String, Double])]((Nil, Map.empty))((acc, ins) => {
                    emulateSingleInstruction(acc._1, acc._2, ins)
                })
            }
            finalEnv._2
        }
}