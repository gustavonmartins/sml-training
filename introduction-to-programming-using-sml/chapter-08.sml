(*8.1 Chinese boxes*)

datatype colour =
	 Red|Blue|Green|Yellow|Purple;

datatype cbox =
	 NONE|
	 Cube of real*colour*cbox;

fun count NONE=0
  | count (Cube(_,_,cbin)) = (count cbin)+1;

exception InvalidCube;

fun insert (r,c,cb)=
  if r <= 0.0
  then
      raise InvalidCube
  else
      case cb of
	  NONE => Cube(r,c,cb)
	| (Cube(r0,_,_)) => case Real.compare(r,r0) of
				LESS => raise InvalidCube
			      | EQUAL => raise InvalidCube
			      | GREATER => Cube(r,c,cb);

fun difflist NONE=[]
  | difflist (Cube(r,c,NONE))=[]
  | difflist (Cube(r,_,cb as Cube(r0,_,_))) = (r-r0)::difflist(cb);


(*8.2 Symbolic differentiation*)

infix 6 ++ --;
infix 7 ** //;

datatype fexpr=
	 Const of real|
	 X|
	 ++ of fexpr*fexpr|
	 -- of fexpr*fexpr|
	 ** of fexpr*fexpr|
	 // of fexpr*fexpr|
	 Sin of fexpr|
	 Cos of fexpr|
	 Ln of fexpr|
	 Exp of fexpr;

fun derivative (Const _) = Const 0.0
  | derivative (X) = Const 1.0
  | derivative (fe1++fe2) = (derivative fe1)++(derivative fe2)
  | derivative (fe1--fe2) = (derivative fe1)--(derivative fe2)
  | derivative (fe1**fe2) = ((fe1)**(derivative fe2))++((derivative fe1)**fe2)
  | derivative (fe1//fe2) = (fe2**(derivative fe1)--(fe1**(derivative fe2)))//(fe2**fe2)
  | derivative (Sin fe) = (Cos fe)**derivative(fe) 
  | derivative (Cos fe) = ((Const ~1.0)**(Sin fe))**(derivative fe)
  | derivative (Ln fe) = (derivative fe)//fe
  | derivative (Exp fe) = (derivative fe)**(Exp fe);

fun compute (Const fe,_)=fe
  | compute (X,r) = r
  | compute (fe1++fe2,r) = compute(fe1,r)+compute(fe2,r)
  | compute (fe1--fe2,r) = compute(fe1,r)-compute(fe2,r)
  | compute (fe1**fe2,r) = compute(fe1,r)*compute(fe2,r)
  | compute (fe1//fe2,r) = compute(fe1,r)/compute(fe2,r)
  | compute (Sin fe, r) = Math.sin(compute(fe,r))
  | compute (Cos fe,r) = Math.cos(compute(fe,r))
  | compute (Ln fe,r) = Math.ln(compute(fe,r))
  | compute (Exp fe,r) = Math.exp(compute(fe,r));

(*8.3*)
(*8.4*)
(*8.5*)
(*8.6 Electrical circuit*)

datatype 'a circuit=
	 Ser of 'a circuit * 'a circuit|
	 Par of 'a circuit * 'a circuit|
	 Single of 'a (*component*);

fun resistance (Single x)=x
  | resistance (Ser(r1,r2)) = resistance r1 + resistance r2
  | resistance (Par(r1,r2)) = 1.0/(1.0/(resistance r1)+1.0/(resistance r2));
