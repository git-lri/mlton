(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* drt (12/15/89) -- the functor should be used during development work,
   but it is wastes space in the release version.

functor ParserGen(structure LrTable : LR_TABLE
                  structure Stream : STREAM) : LR_PARSER =
*)

structure LrParser :> LR_PARSER =
struct

structure LrTable = LrTable
structure Stream = Stream

structure Token : TOKEN =
  struct
      structure LrTable = LrTable
      datatype ('a,'b) token = TOKEN of LrTable.term * ('a * 'b * 'b)
      val sameToken = fn (TOKEN (t,_),TOKEN(t',_)) => t=t'
  end


open LrTable
open Token

val DEBUG1 = true
exception ParseError
exception ParseImpossible of int

type ('a,'b) elem = (state * ('a * 'b * 'b))
type ('a,'b) stack = ('a,'b) elem list

val showState = fn (STATE s) => "STATE " ^ Int.toString s

fun printStack(stack: ('a,'b) elem list, n: int) =
   case stack
     of (state, _) :: rest =>
           (writeln ("          " ^ Int.toString n ^ ": " ^ showState state);
            printStack(rest, n+1)
           )
      | nil => ()

val parse = fn {arg, table, lexer, saction, void, ec = {showTerminal, error, ...}, ...} =>
  let fun prAction(stack as (state, _) :: _, (TOKEN (term,_),_), action) =
             (writeln "Parse: state stack:";
              printStack(stack, 0);
              writeln( "       state="
                     ^ showState state
                     ^ " next="
                     ^ showTerminal term
                     ^ " action="
                     ^ (case action
                          of SHIFT state => "SHIFT " ^ (showState state)
                           | REDUCE i => "REDUCE " ^ (Int.toString i)
                           | ERROR => "ERROR"
                           | ACCEPT => "ACCEPT")))
        | prAction (_,_,_) = ()

      val action = LrTable.action table
      val goto = LrTable.goto table

      fun parseStep(lexPair as (TOKEN (terminal, value as (_,leftPos,_)),lexer),
                    stack as (state,_) :: _) =
          let val nextAction = action (state, terminal)
                    val _ = if DEBUG1 then prAction(stack,lexPair,nextAction)
                            else ()
          in case nextAction
             of SHIFT s => parseStep(Stream.get lexer, (s,value) :: stack)
              | REDUCE i => (case saction(i,leftPos,stack,arg)
                             of (nonterm,value,stack as (state,_) :: _ ) =>
                                 parseStep(lexPair,(goto(state,nonterm),value)::stack)
                              | _ => raise (ParseImpossible 197))
              | ERROR => let val (_,leftPos,rightPos) = value
                         in error("syntax error\n",leftPos,rightPos);
                            raise ParseError
                         end
              | ACCEPT => case stack
                          of (_,(topvalue,_,_)) :: _ =>
                              let val (token,restLexer) = lexPair
                              in (topvalue,Stream.cons(token,restLexer))
                              end
                           | _ => raise (ParseImpossible 202)
          end
        | parseStep _ = raise (ParseImpossible 204)
      val lexPair as (TOKEN (_,(_,leftPos,_)),_) = Stream.get lexer
  in parseStep(lexPair,[(initialState table,(void,leftPos,leftPos))])
  end
end;
