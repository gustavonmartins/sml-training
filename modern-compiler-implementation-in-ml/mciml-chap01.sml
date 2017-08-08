(* Chap 01*)
(* Program 1.5*)

type id = string;

datatype binop = Plus|
		 Minus|
		 Times|
	 Div;

datatype stm = CompoundStm of stm * stm|
	       AssignStm of id * exp|
	       PrintStm of exp list
     and
     exp = IdExp of id|
	   NumExp of int|
	   OpExp of exp * binop * exp|
	   EseqExp of stm * exp;

val prog =
    CompoundStm (AssignStm ("a",
			    OpExp (NumExp 5,
				   Plus,
				   NumExp 3)),
		 CompoundStm (AssignStm ("b",
					 EseqExp (PrintStm [IdExp "a",
							    OpExp (IdExp "a",
								   Minus,
								   NumExp 1)],
						  OpExp (NumExp 10, Times, IdExp "a")
						 )
					),
			      PrintStm [IdExp "b"]
			     )
		);

fun maxint ([x])=x
  | maxint (x::xs) = if x<maxint (xs)
		     then maxint (xs)
		     else x

fun maxargs (PrintStm (x)) = maxint([length x, maxargsexplist x])
  | maxargs (AssignStm(_,e)) = maxargsexplist [e]
  | maxargs (CompoundStm(s1,s2)) = maxint [maxargs s1, maxargs s2]
and maxargsexplist nil = 0
  | maxargsexplist ((e:exp)::nil) = (case e of
					 IdExp(_) => 0
				       | NumExp(_) => 0
				       | EseqExp(s1,e1) => maxint[maxargs s1,maxargsexplist [e1]]
				       | OpExp(l,_,r) => maxint[maxargsexplist [l], maxargsexplist [r]])
  | maxargsexplist (e::es) = maxint [maxargsexplist [e], maxargsexplist es];

type table= (id * int) list;

exception NotFound;

fun update(t:table,i:id,n:int):table=(i,n)::t;

fun lookup((nil:table), _)= raise NotFound
  | lookup ((iT,nT)::ts, i:id) = if iT=i
				 then nT
				 else lookup (ts, i)

					     
fun interpStm (AssignStm(i,e),environment) = (* TODO: Stop requiring manual update of table *)
  let
      val (numbervalue,newenvironment)= interpExp(e,environment)
  in
      update(environment,i,numbervalue)
  end
  | interpStm (CompoundStm(s1,s2),t) = interpStm(s2,interpStm(s1,t))
  | interpStm(PrintStm(espressoes),t:table) =
    let
	val ambienteinicial = t
	fun funcao (currentexp, t)=
	  let
	      val (numero, t_novo) = interpExp(currentexp,t)
	  in
	      (print (Int.toString numero);
	       print " ";
	       t_novo)
	  end;
    in
	(* foldr funcao pontape listadeexpressoes *)
	(print "\n";
	foldl funcao ambienteinicial espressoes)
    end
and    interpExp (IdExp(id),t) = (lookup(t,id),t)
     | interpExp (NumExp(e),t)=(e,t)
     | interpExp (OpExp(NumExp(e1),b,NumExp(e2)),t) = (case b of
							   Plus =>  (e1+e2,t)
							 | Minus => (e1-e2,t)
							 | Times => (e1*e2,t)
							 | Div => (e1 div e2,t)
						      )
     | interpExp (OpExp(IdExp(e1),b,IdExp(e2)),t) = (case b of
							 Plus => (lookup(t,e1)+lookup(t,e2),t)
						       | Minus => (lookup(t,e1)-lookup(t,e2),t)
						       | Times => (lookup(t,e1)*lookup(t,e2),t)
						       | Div => (lookup(t,e1) div lookup(t,e2),t)
						    )
     | interpExp (OpExp(IdExp(e1),b,NumExp(e2)),t) = (case b of
							  Plus => (lookup(t,e1)+e2,t)
							| Minus => (lookup(t,e1)-e2,t)
							| Times => (lookup(t,e1)*e2,t)
							| Div => (lookup(t,e1) div e2,t)
						     )
     | interpExp (OpExp(NumExp(e1),b,IdExp(e2)),t) = (case b of
							  Plus => (e1+lookup(t,e2),t)
							| Minus => (e1-lookup(t,e2),t)
							| Times => (e1*lookup(t,e2),t)
							| Div => (e1 div lookup(t,e2),t)
						     )
     | interpExp (EseqExp(s,e),t) = (interpStm(s,t);
				     interpExp(e,t));

fun interp(s:stm):unit=
  let val environment = []:table
  in
      (interpStm(s,environment);
		print "\nDone interpreting! \n")
  end
      
