(* Modified by Frédéric Tuong
 * Isabelle/C
 * (c) 2018-2019 Université Paris-Saclay, Univ. Paris-Sud, France
 *)
theory MLYacc
  imports Main
begin
ML\<open>open Unsynchronized\<close>
(**)
ML\<open>
     fun foldl f b l =
        let
           fun loop (l, b) =
              case l of
                 [] => b
               | x :: l => loop (l, f (x, b))
        in loop (l, b)
        end
     fun foldr f b l = foldl f b (rev l)
     val concat = String.concat
     val print = writeln
     val implode = String.implode
     val explode = String.explode
\<close>
(**)
ML_file\<open>../../lib/mlyacc-lib0/base.sig\<close>
ML_file\<open>../../lib/mlyacc-lib0/join.sml\<close>
ML_file\<open>../../lib/mlyacc-lib0/lrtable.sml\<close>
ML_file\<open>../../lib/mlyacc-lib0/stream.sml\<close>
ML_file\<open>../../lib/mlyacc-lib0/parser2.sml\<close>
(**)
ML_file\<open>utils.sig\<close>
ML_file\<open>utils.sml\<close>
ML_file\<open>sigs.sml\<close>
ML_file\<open>verbose.sml\<close>
ML_file\<open>hdr.sml\<close>
ML_file\<open>parse.sml\<close>
ML_file\<open>core.sml\<close>
ML_file\<open>coreutils.sml\<close>
ML_file\<open>graph.sml\<close>
ML_file\<open>look.sml\<close>
ML_file\<open>lalr.sml\<close>
ML_file\<open>mklrtable.sml\<close>
ML_file\<open>grammar.sml\<close>
ML_file\<open>mkprstruct.sml\<close>
ML_file\<open>shrink.sml\<close>
ML_file\<open>absyn.sig\<close>
ML_file\<open>yacc.sml\<close>
ML_file\<open>absyn.sml\<close>
(**)
ML_file\<open>../generated/yacc.grm.sig\<close>
ML_file\<open>../generated/yacc.grm.sml\<close>
ML_file\<open>../generated/yacc.lex.sml\<close>
(**)
ML_file\<open>link.sml\<close>
ML\<open>ParseGen.parseGen\<close>

end