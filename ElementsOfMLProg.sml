(*Elements of ML programming*)

(*Exercises 3.1.1*)
fun cubereal x = x*x*x;

fun smallestofthree (a,b,c)=if (a<=b andalso a<=c) then a else
			    if b<=c then b else c;

fun thirdoflist (list) = hd (tl (tl list));

fun revtuple3 (a,b,c) = (c,b,a);

fun thirdcharofstr str= thirdoflist (explode str);

fun cyclelistonce list= (tl list)@((hd list)::nil);

(*Exercises 3.1.2*)
(*a,b,c missing*)
fun secondeldeleted list = (hd list)::(tl (tl list));

(*Chapter 3.2*)
fun reverse L= if L=nil then nil else
	       (reverse (tl L))@((hd L)::nil);

fun comb (n,m) = if (m=0 orelse n=m) then 1
		 else comb(n-1,m) + comb(n-1, m-1);

fun takeoddpos L=
  if L=nil then nil else
  (hd L)::(takeevenpos(tl L))

and
takeevenpos L=
if L=nil then nil else
takeoddpos(tl L);

(*Exercises 3.2.1*)
fun cyclelistn (L, n:int)=
  if n=0 then L
  else if n=1 then cyclelistonce L
  else cyclelistn (cyclelistonce L,n-1);


fun listlength L=
  if L=nil then 0
  else 1+listlength (tl L);

fun mergesmallestfirst (nil, M)= M
  | mergesmallestfirst (L, nil) = L
  | mergesmallestfirst (L as fL::rL, M as fM::rM) =
    if fL <= fM then fL::(mergesmallestfirst (rL,M))
    else fM::(mergesmallestfirst (L,rM));

fun sumPairs(nil)=0
  | sumPairs (nil::otherLists) = sumPairs(otherLists)
  | sumPairs ((x::xs)::otherLists) = x+sumPairs(xs::otherLists);

fun hundrethPower(x:real)=
  let
      val five=x*x*x*x*x;
      val tfive=five*five*five*five*five;
  in
      tfive*tfive*tfive*tfive
  end;

fun split(nil)=(nil,nil)
  | split (a::nil) = ([a],nil)
  | split (a::b::cs) =
    let
	val (M,N)=split(cs)
    in
	(a::M,b::N)
    end;

fun mergesort (nil)=nil
  | mergesort (a::nil) = [a]
  | mergesort (L) =
    let
	val (M,N)=split(L);
    in
	mergesmallestfirst(mergesort M,mergesort N)
    end;

(*Exercises 3.4.5*)
(*skipped*)

fun rev1 (nil,M) = M (*usage: rev1([5,4,3,2,1],nil)*)
  | rev1(x::xs,M)=rev1(xs,x::M); 

fun printList(nil)=nil
  | printList (x::xs) = (
      print(Int.toString(x));
      print("\n");
      printList(xs)
  );

fun readList(infile)=
  if endOfStream(infile)=true then nil
  else inputN(infile,1)::readList(infile);


fun makeList1(infile,NONE)=nil
  | makeList1 (infile,SOME c) = c::makeList1(infile,input1(infile));

fun makeList(infile)=
      makeList1(infile,input1(infile));

(*Chapter 5*)

val rec reverse = fn
		 nil => nil
   |x::xs => reverse(xs)@[x];

val addOne = fn
	    x => x+1;

val double_crazy = fn 0 => 10 | x => 2*x;

fun double_crazy x = case x of
			 0 => 10 
		       | x => 2*x;


(*Exercise 5.1.2*)



val rec sumPairs=fn nil => 0 | (x,y)::xs => x+y+sumPairs(xs);

fun sumPairs x = case x of
		     nil => 0
		   | (x,y)::xs => x+y+sumPairs(xs);

fun printList(nil)=nil
  | printList (x::xs) = (
      print(Int.toString(x));
      print("\n");
      printList(xs)
  );

val rec printList = fn
		   nil => nil
  | (x::xs) => (
      print(Int.toString(x));
      print("\n");
      printList(xs)
  );

fun printlist x = case x of
		      nil => nil
		    | x::xs => (
			print(Int.toString(x));
			print("\n");
			printList(xs)
		    );

val rec mergePartiallySorted = fn
			      (nil,M) => M
					     |(L,nil) => L
   |(L as x::xs, M as y::ys) => if x<y then x::mergePartiallySorted(xs,M) else
				y::mergePartiallySorted(L,ys);

fun mergePartiallySorted (L,M) = case (L,M) of
				     (nil,M) => M
				   | (L,nil) => L
				   | (L as x::xs, M as y::ys) => if x<y then x::mergePartiallySorted(xs,M) else
								 y::mergePartiallySorted(ys,L);

val rec comb = fn
	      (_,0)=>1
   |(n,m) => if n=m then 1 else
	     comb(n-1,m) + comb(n-1,m-1);

fun comb (n,m) = case (n,m) of
		     (_,0) => 1
		   | (n,m) => if n=m then 1 else
			      comb(n-1,m)+comb(n-1,m-1);

(*Exercise 5.1.4*)
