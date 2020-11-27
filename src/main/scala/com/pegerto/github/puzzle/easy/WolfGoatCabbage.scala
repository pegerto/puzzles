package com.pegerto.github.puzzle.easy

import scala.annotation.tailrec

object WolfGoatCabbage {

  sealed trait Traveler
  case object Wolf extends Traveler
  case object Goat extends Traveler
  case object Cabbage extends Traveler

  sealed trait Action
  case object NoMove extends Action
  case class Move(traveler: Traveler) extends Action

  sealed  trait GameError
  case object InvalidAction extends GameError

  val invalidStates = Seq(
    Set(Wolf, Goat),
    Set(Goat,Cabbage)
  )

  case class State(
      left: List[Traveler],
      right: List[Traveler],
      boatLeft: Boolean ) {

    def currentSide: List[Traveler] = if(boatLeft) left else right
    def isDone: Boolean = left.isEmpty
    def isValidState = (if(boatLeft) right else left)
      .combinations(2).map(_.toSet).foldLeft(true)(_ && !invalidStates.contains(_))

    def validActions:List[Action] = {
      val actions = NoMove :: currentSide.map(Move)
      actions.flatMap( m => {
        val futureState = execute(m)
        futureState match {
          case Right(state) if state.isValidState => Some(m)
          case _ => None
        }
      })
    }

    def execute(action: Action): Either[GameError, State] = action match {
      case NoMove  => Right(this.copy(boatLeft = !boatLeft))
      case Move(traveler) if currentSide.contains(traveler) => {
        if (boatLeft) {
          Right(State(left.filter(_ != traveler), traveler :: right, false))
        } else {
          Right(State(traveler :: left, right.filter(_ != traveler), true))
        }
      }
      case _ => Left(InvalidAction)
    }
  }

  @tailrec
  def play(toExplore : List[(State, Action, List[Action])]): Option[List[Action]] = toExplore match {
    case ::( (state, action, history), tail) => {
      if (state.isDone) {
        Some(history)
      } else {
        state.execute(action) match {
          case Right(newState) => play(tail ::: newState.validActions.map((newState, _, history :+ action) ))
          case Left(_) => None
        }
      }
    }
    case Nil => None
  }

  def play(initialState: State): Option[List[Action]] = play(initialState.validActions.map((initialState, _, List.empty)))

  def main(string: Array[String]): Unit = {

    val initialState = State(List(Wolf, Goat, Cabbage), List.empty[Traveler], true)
    val moves = play(initialState)
    println(moves)
  }
}
