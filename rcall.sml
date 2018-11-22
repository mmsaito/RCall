(* example of executing R and send R-code to it *)
structure RCall = struct
  structure U = Unix;
  structure T = TextIO;

  fun start optlst = let
    val proc = U.execute ("/usr/bin/R", ["--no-save", "--interactive"] @ optlst)
                 : (T.instream, T.outstream) U.proc; 
    val is   = U.textInstreamOf proc;
    val os   = U.textOutstreamOf proc;

    fun recv () =  let
      val cs = ref ([]: string list)
      val n = ref 0
      fun check () = 
        (n := getOpt (T.canInput(is, 256), 0); !n)
    in
      ( while check() > 0
         do (cs := T.inputN(is,!n) :: !cs)
         ; String.concat (rev (!cs))
      (* ;  (rev (!cs)) *)
      )
    end

    val send' = fn s => T.output(os,s);
    val send  = fn s => (send' s; send' "\n"; T.flushOut os); 
  in
    {is = is, os = os, send' = send', send = send, recv = recv}
  end
end


(* user code -- draw a sine curve *)
val rproc = RCall.start [];

infix 1 >> $
fun op >> (x,f) = f x
fun op $ (f,x) = f x
val sleep = OS.Process.sleep o Time.fromReal;

(* ex.1: plotting *)
val cmds = [
  "x11(width=3,height=3,xpos = 0, ypos=0)",
  "ts = 1:360",
  "xs = ts*pi/180",
  "ys = sin(xs)",
  "par(mai=c(0.5,0.5,0.5,0.2))",
  "plot(ts,ys,type='l',lwd=2,col='red',main='sin curve')",
  ""
  ];

val zz = app (#send rproc) cmds;

(* ex.2: binary transfer *)
val zz = app (#send rproc)
  [
  "x <- readBin(file(\"stdin\",\"rb\"), \"numeric\", n = 1, size = 8, endian=\"little\")",
  String.implode (map chr [0x18,0x2d,0x44,0x54,0xfb,0x21,0x09,0x40])
  ];

val _ = OS.Process.sleep (Time.fromReal 1.0);
  (* needs a wait for R CLI to be ready again *)

val zz = app (#send rproc)
  [
  "text(180,0, sprintf(\"x = %14.16g\", x))",
  "",
  ""
  ];

(* ex.3: bring R's output with markups of being and end *)
val zz = app $ #send rproc $
  ["NL    = intToUtf8(10)"
  ,"BEGIN = paste(c(NL,rep('@',4),NL),collapse='')"
  ,"END   = paste(c(NL,rep('$',4),NL),collapse='')"
  ];

fun sr cmd = 
  (#send rproc ("cat(BEGIN);" ^ cmd ^ ";cat(END)"); sleep 1.0 ; #recv rproc ());
  (* if update SML/NJ so that you have a shorter pause ... *)

val zz =  sr "print (array(dim=c(4,3),1:12))" >> print;


(* ex.4: read a slider value of gWidgets *)
val zz = app $ #send rproc $
  ["require(gWidgets)"
  ,"w = gwindow('HOG')"
  ,"w.sl = gslider(container=w)"
  ];

val _ = #recv rproc () >> print ;

fun sval () = 
  (sr "cat(svalue(w.sl))" 
  >> String.tokens (fn #"\n" => true | _ => false) 
  >> (fn s => List.nth(s,2)) 
  >> Int.fromString 
  >> valOf
  );

fun watch () = 
  while true do (sval() >> Int.toString >> (fn s => print (s^":")));
