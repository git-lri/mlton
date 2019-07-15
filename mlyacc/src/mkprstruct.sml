(* Modified by Frédéric Tuong
 * Isabelle/C
 * (c) 2018-2019 Université Paris-Saclay, Univ. Paris-Sud, France
 *)
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

functor mkPrintStruct(structure LrTable : LR_TABLE
                      structure ShrinkLrTable : SHRINK_LR_TABLE
                      sharing LrTable = ShrinkLrTable.LrTable):PRINT_STRUCT =
   struct
      val sub = Array.sub
      infix 9 sub
      structure LrTable = LrTable
      open ShrinkLrTable LrTable


      (* lineLength = approximately the largest number of characters to allow
         on a line when printing out an encode string *)

      val lineLength = 72

      (* maxLength = length of a table entry.  All table entries are encoded
         using two 16-bit integers, one for the terminal number and the other
         for the entry.  Each integer is printed as two characters (low byte,
         high byte), using the ML ascii escape sequence.  We need 4
         characters for each escape sequence and 16 characters for each entry
      *)

      val maxLength =  16

      (* number of entries we can fit on a row *)

      val numEntries = lineLength div maxLength

      (* convert integer between 0 and 255 to the three character ascii
         decimal escape sequence for it *)

      val chr =
        let val lookup = Array.array(256,"\000")
            val intToString = fn i =>
                if i>=100 then "\\" ^ (Int.toString i)
                else if i>=10 then "\\0" ^ (Int.toString i)
                else  "\\00" ^ (Int.toString i)
            fun loop n = if n=256 then ()
                         else (Array.update(lookup,n,intToString n); loop (n+1))
        in loop 0; fn i => lookup sub i
        end

      val makeStruct = fn {table,name,print,verbose} =>
       let
         val states = numStates table
         val rules = numRules table
         fun printPairList (prEntry : 'a * 'b -> unit) l =
               let fun f (EMPTY,_) = ()
                     | f (PAIR(a,b,r),count) =
                            if count >= numEntries then
                               (print "\\\n\\"; prEntry(a,b); f(r,1))
                            else (prEntry(a,b); f(r,(count+1)))
               in f(l,0)
               end
         val printList : ('a -> unit) -> 'a list -> unit =
           fn prEntry => fn l =>
                let fun f (nil,_) = ()
                      | f (a :: r,count) =
                             if count >= numEntries then
                                 (print "\\\n\\"; prEntry a; f(r,1))
                                else (prEntry a; f(r,count+1))
                in f(l,0)
                end
         val prEnd = fn _ => print "\\000\\000\\\n\\"
         fun printPairRow prEntry =
               let val printEntries = printPairList prEntry
               in fn l => (printEntries l; prEnd())
               end
         fun printPairRowWithDefault (prEntry,prDefault) =
               let val f = printPairRow prEntry
               in fn (l,default) => (prDefault default; f l)
               end
         fun printTable (printRow,count) =
               (print "\"\\\n\\";
                let fun f i = if i=count then ()
                               else (printRow i; f (i+1))
                in f 0
                end;
                print"\"\n")
         val printChar = print o chr

          (* print an integer between 0 and 2^16-1 as a 2-byte character,
             with the low byte first *)

         val printInt = fn i => (printChar (i mod 256);
                                  printChar (i div 256))

         (* encode actions as integers:

                ACCEPT => 0
                ERROR => 1
                SHIFT i => 2 + i
                REDUCE rulenum => numstates+2+rulenum
         *)

         val printAction =
              fn (REDUCE rulenum) => printInt (rulenum+states+2)
                 | (SHIFT (STATE i)) => printInt (i+2)
                 | ACCEPT => printInt 0
                 | ERROR => printInt 1

         val printTermAction = fn (T t,action) =>
                (printInt (t+1); printAction action)

         val printGoto = fn (NT n,STATE s) => (printInt (n+1); printInt s)

         val ((rowCount,rowNumbers,actionRows),entries)=
                   shrinkActionList(table,verbose)
         val getActionRow = let val a = Array.fromList actionRows
                            in fn i => a sub i
                            end
         val printGotoRow : int -> unit =
               let val f = printPairRow printGoto
                   val g = describeGoto table
               in fn i => f (g (STATE i))
               end
        val printActionRow =
              let val f = printPairRowWithDefault(printTermAction,printAction)
              in fn i => f (getActionRow i)
              end
        in print "val ";
           print name;
           print "=";
           print "let val actionRows =\n";
           printTable(printActionRow,rowCount);
           print "  val actionRowNumbers =\n\"";
           printList (fn i => printInt i) rowNumbers;
           print "\"\n";
           print "  val gotoT =\n";
           printTable(printGotoRow,states);
           print "  val numstates = ";
           print (Int.toString states);
           print "\n  val numrules = ";
           print (Int.toString rules);
           print "\n\
\  datatype acc = Acc of string * int\n\
\\n\
\  fun string_to_int (Acc (s, i)) =\n\
\    (Char.ord (String.sub (s, i)) + Char.ord (String.sub (s, i + 1)) * 256, Acc (s, i + 2))\n\
\\n\
\  fun string_to_table string_to s =\n\
\    let val len = String.size s\n\
\        fun f (Acc (s, index)) = (Acc (s, index)) |> (if index < len then (string_to ::: f)\n\
\                                                      else Scan.succeed nil)\n\
\    in Acc (s, 0) |> f |> fst end\n\
\\n\
\  fun string_to_pairlist conv_key conv_entry =\n\
\    let fun f acc = acc |>\n\
\      (string_to_int\n\
\       :|-- (fn 0 => Scan.succeed EMPTY\n\
\              | n => string_to_int -- f >> (fn (i, xs) => PAIR (conv_key (n - 1), conv_entry i, xs))))\n\
\    in f end\n\
\\n\
\  fun string_to_pairlist_T conv_entry =\n\
\    string_to_int -- string_to_pairlist T conv_entry >> (swap #> apsnd conv_entry)\n\
\\n\
\  local\n\
\    val memo = Array.array (numstates + numrules, ERROR)\n\
\    val _ = let fun g i = (Array.update (memo, i, REDUCE (i - numstates)); g (i + 1))\n\
\                fun f i = if i = numstates then g i\n\
\                          else (Array.update (memo, i, SHIFT (STATE i)); f (i + 1))\n\
\            in f 0 handle General.Subscript => () end\n\
\  in val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub (memo, j - 2) end\n\
\in\n\
\  LALR_Table.mkLrTable\n\
\    {actions =\n\
\      Array.fromList\n\
\       (map (curry Array.sub (Array.fromList (string_to_table (string_to_pairlist_T entry_to_action)\n\
\                                                              actionRows)))\n\
\            (string_to_table string_to_int actionRowNumbers)),\n\
\     gotos = Array.fromList (string_to_table (string_to_pairlist NT STATE) gotoT),\n\
\     numRules = numrules,\n\
\     numStates = numstates,\n\
\     initialState = STATE ";
print (Int.toString ((fn (STATE i) => i) (initialState table)));
print "}\nend\n";
      entries
      end
end;
