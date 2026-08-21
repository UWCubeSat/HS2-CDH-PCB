(* ::Package:: *)

Clear[R1, R2, R3, VOV, VUV, VOVPR, VENR]
sol = Solve[
   {
    VOVPR == R3/(R1 + R2 + R3)*VOV,
    VENR == (R2 + R3)/(R1 + R2 + R3)*VUV
   },
   {R1, R2}
   ];

(* substitute your numeric thresholds and a candidate R3 *)
sol /. {VOVPR -> 0.99, VOV -> 5.5, VENR -> 0.99, VUV -> 4.5, R3 -> 31200}





Clear[R4, R5, VPGTH]
sol = Solve[VPGTH == 0.99*(1 + R4/R5), R5];
sol /. {VPGTH -> 4.5, R4 -> 475000}


(* Constants from datasheet *)
gainIMON = 52*10^-6;      (* A/A, IOUT-to-IMON current gain *)
iIMONos = 0.8*10^-6;      (* A, IMON offset current, typical *)

(* Your system parameters *)
vIN = 5;                   (* V, nominal input *)
iLIM = 2.49;                (* A, your actual set current limit (R18=35.7k) *)

(* Equation 5: max R_IMON to keep V_IMON in its linear range *)
numeratorClamp = Min[vIN - 2.2, 6];  (* V *)
rIMONmax = numeratorClamp / (1.6 * iLIM * gainIMON);
Print["R_IMON max = ", rIMONmax, " ohms"]

(* Pick a candidate R_IMON safely under that ceiling *)
rIMON = 13300;  (* E96 value, ~1.6% margin under the computed max *)

(* Equation 6: V_IMON at a given load current *)
vIMON[iOUT_] := (iOUT*gainIMON + iIMONos) * rIMON

(* Check across your expected operating range *)
Table[{iOUT, vIMON[iOUT]}, {iOUT, {0.5, 1.85, 2.05, iLIM}}]



Clear[R1, R2, R3, VOVLO, VUVLO, VINOV, VINUV]

sol = Solve[
   {
    VOVLO == R3/(R1 + R2 + R3)*VINOV,
    VUVLO == (R2 + R3)/(R1 + R2 + R3)*VINUV
   },
   {R1, R2}
   ];

result = sol /. {VOVLO -> 1.2, VINOV -> 3.6, VUVLO -> 1.2, VINUV -> 3.0, R3 -> 47000}

(* Divider current check - confirm this clears 20x any expected leakage *)
{r1, r2} = {R1, R2} /. result[[1]];
iDivider = 3.3/(r1 + r2 +47000)*10^6  (* \[Micro]A, using 3.3V nominal rail *)



