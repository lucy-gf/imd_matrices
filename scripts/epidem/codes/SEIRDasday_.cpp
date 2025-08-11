//SEIRDas

#include <Rcpp.h>
using namespace Rcpp;
//using namespace std;
#include <array>
// [[Rcpp::export]]
List model(List parscpp) { 

  //natural history parameters
  const double beta( parscpp["beta"]);     //probability of infection of susceptible upon contact x contact renormalisation 
  const double rEI(  parscpp["rEI"]);      //1/latent period - time to infection
  const double rI1I2(parscpp["rI1I2"]);    //1/time to clinical symptoms
  const double rI2R( parscpp["rI2R"]);     //1/infectious period of clinical infections
  const double rUR(  parscpp["rUR"]);      //1/infectious period of sub-clinical infections
  const double rE    = 2*rEI;  
  const double rU    = 2*rUR;  
  const double f(    parscpp["f"]);        //infectiousness of sub-clinical relative to clinical
  //integration parameters
  const double dt(   parscpp["dt"]);       //time step for integration
  const int    nt(   parscpp["nt"]);       //number of time steps
  const int    nd(   parscpp["nd"]);       //number of days (start of day, to end of last day)
  const int    na(   parscpp["na"]);       //number of age groups
  const int    ns(   parscpp["nimd"]);     //number of se groups, e.g. imd
  const int    ng = na*ns;                 //number of age x se groups
  
  //it index for start of each day
  IntegerVector iW(nd);
  //vector parameters
  const std::vector<double> y45(parscpp["y45"]); //critical fraction age x SES
  const std::vector<double>  u( parscpp["u"]);   //susceptibility
  const std::vector<double> mI( parscpp["mI"]);  //rate of mortality of clinically infected
  const std::vector<double> rrep( parscpp["rrep"]); //rate of reporting
  
  // Contact matrices
  const std::vector<double> cm( parscpp["cm"]);  //ng x ng matrix - rows=participant, cols=contact
  const int   cmdim1( parscpp["cmdim1"]);  //row dimension of cm
  
  // initial states at time[0] - age-vector
  const std::vector<double> Sg0( parscpp["Sg0"]);  // Susceptible
  const std::vector<double> E1g0(parscpp["E1g0"]); // Exposed TODO: update E1, E2
  const std::vector<double> E2g0(parscpp["E1g0"]); // Exposed
  const std::vector<double> U1g0(parscpp["U1g0"]); // Sub-clinical cases TODO: update U1, U2
  const std::vector<double> U2g0(parscpp["U2g0"]); // Sub-clinical cases
  const std::vector<double> I1g0(parscpp["I1g0"]); // Pre-clinical cases TODO: update I1, I2
  const std::vector<double> I2g0(parscpp["I2g0"]); // Clinical cases
  const std::vector<double> Rg0(parscpp["Rg0"]);   // Recovered
  const std::vector<double> Dg0(parscpp["Dg0"]);   // Dead
  const std::vector<double> oNg(parscpp["oNg"]);   // 1/number per age group - constant over time
  
  // States(age,time) = 0 - need initialise S(na,0) etc
  std::vector<double> S_0(ng);  //current and next time step
  std::vector<double> E1_0(ng); //vector requires #include array
  std::vector<double> E2_0(ng);  
  std::vector<double> I1_0(ng);  
  std::vector<double> I2_0(ng);   
  std::vector<double> U1_0(ng);   
  std::vector<double> U2_0(ng);   
  std::vector<double>  R_0(ng);   
  std::vector<double>  D_0(ng);  
  std::vector<double> Cc_0(ng);  
  
  // States(time) = 0 - need initialise St[0] etc, time[0]
  NumericVector time(nt);
  std::vector<double> St(nt); //TODO: use NumericVector if returning at the end
  std::vector<double> E1t(nt); 
  std::vector<double> E2t(nt); 
  std::vector<double> I1t(nt); 
  std::vector<double> I2t(nt); 
  std::vector<double> U1t(nt); 
  std::vector<double> U2t(nt); 
  std::vector<double> Rt(nt);  
  std::vector<double> Dt(nt);  
  std::vector<double> Cct(nt); //cumulative clinical cases
  
//LATER: daily by age
  NumericVector Sw(nd); //= Rcpp::clone(Sw);
  NumericVector Ew(nd);  
  NumericVector Uw(nd); 
  NumericVector Iw(nd); 
  NumericVector Rw(nd); 
  NumericVector Dw(nd); 
  NumericVector Ccw(nd);
  NumericVector Ew_s1(nd); //SES
  NumericVector Ew_s2(nd);
  NumericVector Ew_s3(nd);
  NumericVector Ew_s4(nd);
  NumericVector Ew_s5(nd);
  NumericVector Iw_s1(nd);
  NumericVector Iw_s2(nd);
  NumericVector Iw_s3(nd);
  NumericVector Iw_s4(nd);
  NumericVector Iw_s5(nd);
  NumericVector Uw_s1(nd);
  NumericVector Uw_s2(nd);
  NumericVector Uw_s3(nd);
  NumericVector Uw_s4(nd);
  NumericVector Uw_s5(nd);
  NumericVector Rw_s1(nd);
  NumericVector Rw_s2(nd);
  NumericVector Rw_s3(nd);
  NumericVector Rw_s4(nd);
  NumericVector Rw_s5(nd); 
  NumericVector Iw_a1(nd); //age
  NumericVector Iw_a2(nd);
  NumericVector Iw_a3(nd);
  NumericVector Iw_a4(nd);
  NumericVector Iw_a5(nd);
  NumericVector Iw_a6(nd);
  NumericVector Iw_a7(nd);
  NumericVector Iw_a8(nd);
  NumericVector Iw_a9(nd);
  NumericVector Iw_a10(nd);
  NumericVector Iw_a11(nd);
  NumericVector Iw_a12(nd);
  NumericVector Iw_a13(nd);
  NumericVector Iw_a14(nd);
  NumericVector Iw_a15(nd);
  NumericVector Iw_a16(nd);
  NumericVector Uw_a1(nd);
  NumericVector Uw_a2(nd);
  NumericVector Uw_a3(nd);
  NumericVector Uw_a4(nd);
  NumericVector Uw_a5(nd);
  NumericVector Uw_a6(nd);
  NumericVector Uw_a7(nd);
  NumericVector Uw_a8(nd);
  NumericVector Uw_a9(nd);
  NumericVector Uw_a10(nd);
  NumericVector Uw_a11(nd);
  NumericVector Uw_a12(nd);
  NumericVector Uw_a13(nd);
  NumericVector Uw_a14(nd);
  NumericVector Uw_a15(nd);
  NumericVector Uw_a16(nd);
  NumericVector Iw_g1(nd); //all
  NumericVector Iw_g2(nd);
  NumericVector Iw_g3(nd);
  NumericVector Iw_g4(nd);
  NumericVector Iw_g5(nd);
  NumericVector Iw_g6(nd);
  NumericVector Iw_g7(nd);
  NumericVector Iw_g8(nd);
  NumericVector Iw_g9(nd);
  NumericVector Iw_g10(nd);
  NumericVector Iw_g11(nd);
  NumericVector Iw_g12(nd);
  NumericVector Iw_g13(nd);
  NumericVector Iw_g14(nd);
  NumericVector Iw_g15(nd);
  NumericVector Iw_g16(nd);
  NumericVector Iw_g17(nd);
  NumericVector Iw_g18(nd);
  NumericVector Iw_g19(nd);
  NumericVector Iw_g20(nd);
  NumericVector Iw_g21(nd);
  NumericVector Iw_g22(nd);
  NumericVector Iw_g23(nd);
  NumericVector Iw_g24(nd);
  NumericVector Iw_g25(nd);
  NumericVector Iw_g26(nd);
  NumericVector Iw_g27(nd);
  NumericVector Iw_g28(nd);
  NumericVector Iw_g29(nd);
  NumericVector Iw_g30(nd);
  NumericVector Iw_g31(nd);
  NumericVector Iw_g32(nd);
  NumericVector Iw_g33(nd);
  NumericVector Iw_g34(nd);
  NumericVector Iw_g35(nd);
  NumericVector Iw_g36(nd);
  NumericVector Iw_g37(nd);
  NumericVector Iw_g38(nd);
  NumericVector Iw_g39(nd);
  NumericVector Iw_g40(nd);
  NumericVector Iw_g41(nd);
  NumericVector Iw_g42(nd);
  NumericVector Iw_g43(nd);
  NumericVector Iw_g44(nd);
  NumericVector Iw_g45(nd);
  NumericVector Iw_g46(nd);
  NumericVector Iw_g47(nd);
  NumericVector Iw_g48(nd);
  NumericVector Iw_g49(nd);
  NumericVector Iw_g50(nd);
  NumericVector Iw_g51(nd);
  NumericVector Iw_g52(nd);
  NumericVector Iw_g53(nd);
  NumericVector Iw_g54(nd);
  NumericVector Iw_g55(nd);
  NumericVector Iw_g56(nd);
  NumericVector Iw_g57(nd);
  NumericVector Iw_g58(nd);
  NumericVector Iw_g59(nd);
  NumericVector Iw_g60(nd);
  NumericVector Iw_g61(nd);
  NumericVector Iw_g62(nd);
  NumericVector Iw_g63(nd);
  NumericVector Iw_g64(nd);
  NumericVector Iw_g65(nd);
  NumericVector Iw_g66(nd);
  NumericVector Iw_g67(nd);
  NumericVector Iw_g68(nd);
  NumericVector Iw_g69(nd);
  NumericVector Iw_g70(nd);
  NumericVector Iw_g71(nd);
  NumericVector Iw_g72(nd);
  NumericVector Iw_g73(nd);
  NumericVector Iw_g74(nd);
  NumericVector Iw_g75(nd);
  NumericVector Iw_g76(nd);
  NumericVector Iw_g77(nd);
  NumericVector Iw_g78(nd);
  NumericVector Iw_g79(nd);
  NumericVector Iw_g80(nd);
  
  int ig;
  double Sig, E1ig, E2ig, I1ig, I2ig, U1ig, U2ig, Rig, Dig;
  
  // age group and population states initialised
  for (int is = 0; is < ns; is++) { //ses
  for (int ia = 0; ia < na; ia++) { //age
    ig = ia + is*na;       
    Sig = Sg0[ig]; E1ig = E1g0[ig]; E2ig = E2g0[ig]; I1ig = I1g0[ig]; I2ig = I2g0[ig]; U1ig = U1g0[ig]; U2ig = U2g0[ig]; Rig = Rg0[ig]; Dig = Dg0[ig];
      //Assuming: each imd using same by-age-IC
      S_0[ig]  = Sig;        St[0]  += Sig;
      E1_0[ig] = E1ig;       E1t[0] += E1ig;
      E2_0[ig] = E2ig;       E2t[0] += E2ig;
      I1_0[ig] = I1ig;       I1t[0] += I1ig;
      I2_0[ig] = I2ig;       I2t[0] += I2ig;
      U1_0[ig] = U1ig;       U1t[0] += U1ig;
      U2_0[ig] = U2ig;       U2t[0] += U2ig;
      R_0[ig]  = Rig;        Rt[0]  += Rig;
      D_0[ig]  = Dig;        Dt[0]  += Dig;
      //TODO: Ew, Iw, Uw,
  }} //is, ia

  // Dynamics of state variables - fine Euler integration
  time[0] = 0;
  int  day  = 1;
  int  day0 = 1;
  
  double Spw = 0,   Epw = 0,   Upw = 0,   Ipw = 0,   Rpw=0,   Dpw = 0,   Ccpw = 0;     
  NumericVector Epw_g(ng),   Upw_g(ng),   Ipw_g(ng),   Rpw_g(ng);
  NumericVector Epw_s(ns),   Upw_s(ns),   Ipw_s(ns),   Rpw_s(ns);
  NumericVector Upw_a(na),   Ipw_a(na);

  iW[0]      = 0;

  double Sat, E1at, E2at, U1at, U2at, I1at, I2at, Rat, Dat; //alternative temp-variable declaration
  double dS, dE1, dE2, dI1, dI2, dU1, dU2, dR, dD, dCc, FOI, FOIS; 
  double yas, ua, mIa, rrepa, cmi;
  int icm, ig2;
  double rE1, rE2, rU1, rU2, rI1, rI2, dEin, dIin, dUin;
  
  for (int it = 0; it < (nt-1); it++) {	//Crucial: nt-1 ////////////////////////
    day0 = day;
    day  = 1 + (int) time[it];

    for (int is = 0; is < ns; is++) { //////////////////////////////////////////
    for (int ia = 0; ia < na; ia++) { //////////////////////////////////////////
      ig = is*na + ia; 
      // vector parameters
      yas  = y45[ig];
      ua   = u[ia];
      mIa  = mI[ia];
      rrepa= rrep[ia];
      // current matrix cells
      Sat  = S_0[ig];
      E1at = E1_0[ig];
      E2at = E2_0[ig];
      I1at = I1_0[ig];
      I2at = I2_0[ig];
      U1at = U1_0[ig];
      U2at = U2_0[ig];
      Rat  =  R_0[ig];
      Dat  =  D_0[ig];

      // force of infection on group ia
      FOI = 0;
      //double beta_ua  = beta_infectivity*u[ia];
      for (int is2 = 0; is2 < ns; is2++) {
      for (int ia2 = 0; ia2 < na; ia2++) {
           ig2 = is2*na + ia2; 
           icm = ig2*cmdim1 + ig; //icm = ib*cmdim1 + ia;
           cmi = cm[icm];        //as<NumericVector>(parscpp["cm"])[icm]; //cm[icm];
           FOI       += beta*ua*cmi*( I1_0[ig2] + I2_0[ig2] + f*U1_0[ig2] + f*U2_0[ig2] )*oNg[ig2]; //TODO:UPDATE oNA
      }} //ir, ib
      //Infection update for next timestep
             FOIS = FOI*Sat, rE1 = rE*E1at,      rE2 =   rE*E2at, 
                             rU1 = rU*U1at,      rU2 =   rU*U2at, 
                             rI1 = rI1I2*I1at,   rI2 = rI2R*I2at,
                             dEin = dt*FOIS, 
                             dIin = dt*yas*rE2, 
                             dUin = dt*(1-yas)*rE2;
      dS  = dt*( -FOIS            );
      dE1 = dt*(  FOIS      - rE1 );
      dE2 = dt*(  rE1       - rE2 );
      dI1 = dt*(  yas*rE2  - rI1 );
      dI2 = dt*(  rI1       - rI2 );
      dU1 = dt*(  (1-yas)*rE2- rU1);
      dU2 = dt*(  rU1       - rU2 );
      dR  = dt*(  (1-mIa)*rI2 + rU2);
      dD  = dt*(  mIa*rI2         );
      dCc = dt*(  yas*rE2*rrepa   );

      S_0[ig]  = Sat  + dS;
      E1_0[ig] = E1at + dE1;
      E2_0[ig] = E2at + dE2;
      I1_0[ig] = I1at + dI1;
      I2_0[ig] = I2at + dI2;
      U1_0[ig] = U1at + dU1;
      U2_0[ig] = U2at + dU2;
      R_0[ig]  = Rat  + dR;
      D_0[ig]  = Dat  + dD;
      Cc_0[ig] = Cc_0[ig] + dCc;
      // time counters
      time[it+1]  = (it+1)*dt;
      day        = 1 + (int) time[it+1];
      // population states evaluated (t>0)
      St[it+1]  += Sat  + dS;
      E1t[it+1] += E1at + dE1;
      E2t[it+1] += E2at + dE2;
      I1t[it+1] += I1at + dI1;
      I2t[it+1] += I2at + dI2;
      U1t[it+1] += U1at + dU1;
      U2t[it+1] += U2at + dU2;
      Rt[it+1]  += Rat  + dR;
      Dt[it+1]  += Dat  + dD;
      Cct[it+1] += Cct[it] + dCc;
      // day incidence (pw) 
      Spw       += dS;
      Epw       += dEin;
      Upw       += dUin;
      Ipw       += dIin;
      Rpw       += dR;
      Dpw       += dD;
      Ccpw      += dCc;
      // day incidence - SE stratified - each is=1:5 - add age 1:9
      Epw_s[is]  +=  dEin;
      Ipw_s[is]  +=  dIin;
      Upw_s[is]  +=  dUin;
      Rpw_s[is]  +=  dR;
      // day incidence - age stratified - each ia=1:9 - add is=1:5
      Ipw_a[ia]  +=  dIin;
      Upw_a[ia]  +=  dUin;
      // day incidence - all groups
      Ipw_g[ig]  +=  dIin;
      Upw_g[ig]  +=  dUin;
      }; //ia ////////////////////////////////////////////////////////////////////
    }; //is ////////////////////////////////////////////////////////////////////
    
    if (day - day0 == 1) { //day incidence - update at the end of each day
      iW[day-1]  = it+1;
      Sw[day-1]  = Spw;  Spw=0;
      Ew[day-1]  = Epw;  Epw=0; 
      Iw[day-1]  = Ipw;  Ipw=0; 
      Uw[day-1]  = Upw;  Upw=0; 
      Rw[day-1]  = Rpw;  Rpw=0; 
      Dw[day-1]  = Dpw;  Dpw=0;
      Ccw[day-1] = Ccpw; Ccpw=0;
      //SES
      Ew_s1[day-1] = Epw_s[0]; Epw_s[0]=0;
      Ew_s2[day-1] = Epw_s[1]; Epw_s[1]=0;
      Ew_s3[day-1] = Epw_s[2]; Epw_s[2]=0;
      Ew_s4[day-1] = Epw_s[3]; Epw_s[3]=0;
      Ew_s5[day-1] = Epw_s[4]; Epw_s[4]=0;
      Iw_s1[day-1] = Ipw_s[0]; Ipw_s[0]=0;
      Iw_s2[day-1] = Ipw_s[1]; Ipw_s[1]=0;
      Iw_s3[day-1] = Ipw_s[2]; Ipw_s[2]=0;
      Iw_s4[day-1] = Ipw_s[3]; Ipw_s[3]=0;
      Iw_s5[day-1] = Ipw_s[4]; Ipw_s[4]=0;
      Uw_s1[day-1] = Upw_s[0]; Upw_s[0]=0;
      Uw_s2[day-1] = Upw_s[1]; Upw_s[1]=0;
      Uw_s3[day-1] = Upw_s[2]; Upw_s[2]=0;
      Uw_s4[day-1] = Upw_s[3]; Upw_s[3]=0;
      Uw_s5[day-1] = Upw_s[4]; Upw_s[4]=0;
      Rw_s1[day-1] = Rpw_s[0]; Rpw_s[0]=0;
      Rw_s2[day-1] = Rpw_s[1]; Rpw_s[1]=0;
      Rw_s3[day-1] = Rpw_s[2]; Rpw_s[2]=0;
      Rw_s4[day-1] = Rpw_s[3]; Rpw_s[3]=0;
      Rw_s5[day-1] = Rpw_s[4]; Rpw_s[4]=0;
      //age
      Iw_a1[day-1] = Ipw_a[0]; Ipw_a[0]=0;
      Iw_a2[day-1] = Ipw_a[1]; Ipw_a[1]=0;
      Iw_a3[day-1] = Ipw_a[2]; Ipw_a[2]=0;
      Iw_a4[day-1] = Ipw_a[3]; Ipw_a[3]=0;
      Iw_a5[day-1] = Ipw_a[4]; Ipw_a[4]=0;
      Iw_a6[day-1] = Ipw_a[5]; Ipw_a[5]=0;
      Iw_a7[day-1] = Ipw_a[6]; Ipw_a[6]=0;
      Iw_a8[day-1] = Ipw_a[7]; Ipw_a[7]=0;
      Iw_a9[day-1] = Ipw_a[8]; Ipw_a[8]=0;
      Iw_a10[day-1] = Ipw_a[9]; Ipw_a[9]=0;
      Iw_a11[day-1] = Ipw_a[10]; Ipw_a[10]=0;
      Iw_a12[day-1] = Ipw_a[11]; Ipw_a[11]=0;
      Iw_a13[day-1] = Ipw_a[12]; Ipw_a[12]=0;
      Iw_a14[day-1] = Ipw_a[13]; Ipw_a[13]=0;
      Iw_a15[day-1] = Ipw_a[14]; Ipw_a[14]=0;
      Iw_a16[day-1] = Ipw_a[15]; Ipw_a[15]=0;
      Uw_a1[day-1] = Upw_a[0]; Upw_a[0]=0;
      Uw_a2[day-1] = Upw_a[1]; Upw_a[1]=0;
      Uw_a3[day-1] = Upw_a[2]; Upw_a[2]=0;
      Uw_a4[day-1] = Upw_a[3]; Upw_a[3]=0;
      Uw_a5[day-1] = Upw_a[4]; Upw_a[4]=0;
      Uw_a6[day-1] = Upw_a[5]; Upw_a[5]=0;
      Uw_a7[day-1] = Upw_a[6]; Upw_a[6]=0;
      Uw_a8[day-1] = Upw_a[7]; Upw_a[7]=0;
      Uw_a9[day-1] = Upw_a[8]; Upw_a[8]=0;
      Uw_a10[day-1] = Upw_a[9]; Upw_a[9]=0;
      Uw_a11[day-1] = Upw_a[10]; Upw_a[10]=0;
      Uw_a12[day-1] = Upw_a[11]; Upw_a[11]=0;
      Uw_a13[day-1] = Upw_a[12]; Upw_a[12]=0;
      Uw_a14[day-1] = Upw_a[13]; Upw_a[13]=0;
      Uw_a15[day-1] = Upw_a[14]; Upw_a[14]=0;
      Uw_a16[day-1] = Upw_a[15]; Upw_a[15]=0;
      //all groups
      Iw_g1[day-1] = Ipw_g[0]; Ipw_g[0]=0;
      Iw_g2[day-1] = Ipw_g[1]; Ipw_g[1]=0;
      Iw_g3[day-1] = Ipw_g[2]; Ipw_g[2]=0;
      Iw_g4[day-1] = Ipw_g[3]; Ipw_g[3]=0;
      Iw_g5[day-1] = Ipw_g[4]; Ipw_g[4]=0;
      Iw_g6[day-1] = Ipw_g[5]; Ipw_g[5]=0;
      Iw_g7[day-1] = Ipw_g[6]; Ipw_g[6]=0;
      Iw_g8[day-1] = Ipw_g[7]; Ipw_g[7]=0;
      Iw_g9[day-1] = Ipw_g[8]; Ipw_g[8]=0;
      Iw_g10[day-1] = Ipw_g[9]; Ipw_g[9]=0;
      Iw_g11[day-1] = Ipw_g[10]; Ipw_g[10]=0;
      Iw_g12[day-1] = Ipw_g[11]; Ipw_g[11]=0;
      Iw_g13[day-1] = Ipw_g[12]; Ipw_g[12]=0;
      Iw_g14[day-1] = Ipw_g[13]; Ipw_g[13]=0;
      Iw_g15[day-1] = Ipw_g[14]; Ipw_g[14]=0;
      Iw_g16[day-1] = Ipw_g[15]; Ipw_g[15]=0;
      Iw_g17[day-1] = Ipw_g[16]; Ipw_g[16]=0;
      Iw_g18[day-1] = Ipw_g[17]; Ipw_g[17]=0;
      Iw_g19[day-1] = Ipw_g[18]; Ipw_g[18]=0;
      Iw_g20[day-1] = Ipw_g[19]; Ipw_g[19]=0;
      Iw_g21[day-1] = Ipw_g[20]; Ipw_g[20]=0;
      Iw_g22[day-1] = Ipw_g[21]; Ipw_g[21]=0;
      Iw_g23[day-1] = Ipw_g[22]; Ipw_g[22]=0;
      Iw_g24[day-1] = Ipw_g[23]; Ipw_g[23]=0;
      Iw_g25[day-1] = Ipw_g[24]; Ipw_g[24]=0;
      Iw_g26[day-1] = Ipw_g[25]; Ipw_g[25]=0;
      Iw_g27[day-1] = Ipw_g[26]; Ipw_g[26]=0;
      Iw_g28[day-1] = Ipw_g[27]; Ipw_g[27]=0;
      Iw_g29[day-1] = Ipw_g[28]; Ipw_g[28]=0;
      Iw_g30[day-1] = Ipw_g[29]; Ipw_g[29]=0;
      Iw_g31[day-1] = Ipw_g[30]; Ipw_g[30]=0;
      Iw_g32[day-1] = Ipw_g[31]; Ipw_g[31]=0;
      Iw_g33[day-1] = Ipw_g[32]; Ipw_g[32]=0;
      Iw_g34[day-1] = Ipw_g[33]; Ipw_g[33]=0;
      Iw_g35[day-1] = Ipw_g[34]; Ipw_g[34]=0;
      Iw_g36[day-1] = Ipw_g[35]; Ipw_g[35]=0;
      Iw_g37[day-1] = Ipw_g[36]; Ipw_g[36]=0;
      Iw_g38[day-1] = Ipw_g[37]; Ipw_g[37]=0;
      Iw_g39[day-1] = Ipw_g[38]; Ipw_g[38]=0;
      Iw_g40[day-1] = Ipw_g[39]; Ipw_g[39]=0;
      Iw_g41[day-1] = Ipw_g[40]; Ipw_g[40]=0;
      Iw_g42[day-1] = Ipw_g[41]; Ipw_g[41]=0;
      Iw_g43[day-1] = Ipw_g[42]; Ipw_g[42]=0;
      Iw_g44[day-1] = Ipw_g[43]; Ipw_g[43]=0;
      Iw_g45[day-1] = Ipw_g[44]; Ipw_g[44]=0;
      Iw_g46[day-1] = Ipw_g[45]; Ipw_g[45]=0;
      Iw_g47[day-1] = Ipw_g[46]; Ipw_g[46]=0;
      Iw_g48[day-1] = Ipw_g[47]; Ipw_g[47]=0;
      Iw_g49[day-1] = Ipw_g[48]; Ipw_g[48]=0;
      Iw_g50[day-1] = Ipw_g[49]; Ipw_g[49]=0;
      Iw_g51[day-1] = Ipw_g[50]; Ipw_g[50]=0;
      Iw_g52[day-1] = Ipw_g[51]; Ipw_g[51]=0;
      Iw_g53[day-1] = Ipw_g[52]; Ipw_g[52]=0;
      Iw_g54[day-1] = Ipw_g[53]; Ipw_g[53]=0;
      Iw_g55[day-1] = Ipw_g[54]; Ipw_g[54]=0;
      Iw_g56[day-1] = Ipw_g[55]; Ipw_g[55]=0;
      Iw_g57[day-1] = Ipw_g[56]; Ipw_g[56]=0;
      Iw_g58[day-1] = Ipw_g[57]; Ipw_g[57]=0;
      Iw_g59[day-1] = Ipw_g[58]; Ipw_g[58]=0;
      Iw_g60[day-1] = Ipw_g[59]; Ipw_g[59]=0;
      Iw_g61[day-1] = Ipw_g[60]; Ipw_g[60]=0;
      Iw_g62[day-1] = Ipw_g[61]; Ipw_g[61]=0;
      Iw_g63[day-1] = Ipw_g[62]; Ipw_g[62]=0;
      Iw_g64[day-1] = Ipw_g[63]; Ipw_g[63]=0;
      Iw_g65[day-1] = Ipw_g[64]; Ipw_g[64]=0;
      Iw_g66[day-1] = Ipw_g[65]; Ipw_g[65]=0;
      Iw_g67[day-1] = Ipw_g[66]; Ipw_g[66]=0;
      Iw_g68[day-1] = Ipw_g[67]; Ipw_g[67]=0;
      Iw_g69[day-1] = Ipw_g[68]; Ipw_g[68]=0;
      Iw_g70[day-1] = Ipw_g[69]; Ipw_g[69]=0;
      Iw_g71[day-1] = Ipw_g[70]; Ipw_g[70]=0;
      Iw_g72[day-1] = Ipw_g[71]; Ipw_g[71]=0;
      Iw_g73[day-1] = Ipw_g[72]; Ipw_g[72]=0;
      Iw_g74[day-1] = Ipw_g[73]; Ipw_g[73]=0;
      Iw_g75[day-1] = Ipw_g[74]; Ipw_g[74]=0;
      Iw_g76[day-1] = Ipw_g[75]; Ipw_g[75]=0;
      Iw_g77[day-1] = Ipw_g[76]; Ipw_g[76]=0;
      Iw_g78[day-1] = Ipw_g[77]; Ipw_g[77]=0;
      Iw_g79[day-1] = Ipw_g[78]; Ipw_g[78]=0;
      Iw_g80[day-1] = Ipw_g[79]; Ipw_g[79]=0;
    }

  }; //it //////////////////////////////////////////////////////////////////////

  
  Rcpp::DataFrame byw = Rcpp::DataFrame::create(
    Named("iW")   = iW,
    Named("time") = time[iW],
    //Named("St")   = St[iW],
    //Named("Et")   = E1t[iW]+E2t[iW],
    //Named("It")   = I1t[iW]+I2t[iW],
    //Named("Ut")   = U1t[iW]+U2t[iW],
    //Named("Dt")   = Dt[iW],
    Named("Sw")   = Sw,
    Named("Ew")   = Ew,
    Named("Iw")   = Iw,
    Named("Uw")   = Uw,
    Named("Dw")   = Dw,
    //Named("Rw")   = Rw,
    Named("Iw_s1")  = Iw_s1,
    Named("Iw_s2")  = Iw_s2,
    Named("Iw_s3")  = Iw_s3,
    Named("Iw_s4")  = Iw_s4,
    Named("Iw_s5")  = Iw_s5,
    Named("IUw_s1") = Uw_s1+Iw_s1,
    Named("IUw_s2") = Uw_s2+Iw_s2,
    Named("IUw_s3") = Uw_s3+Iw_s3,
    Named("IUw_s4") = Uw_s4+Iw_s4,
    Named("IUw_s5") = Uw_s5+Iw_s5);
    //Named("Ccw")  = Ccw);
  Rcpp::DataFrame byaw = Rcpp::DataFrame::create(
    Named("IUw_a1") = Uw_a1+Iw_a1,
    Named("IUw_a2") = Uw_a2+Iw_a2,
    Named("IUw_a3") = Uw_a3+Iw_a3,
    Named("IUw_a4") = Uw_a4+Iw_a4,
    Named("IUw_a5") = Uw_a5+Iw_a5,
    Named("IUw_a6") = Uw_a6+Iw_a6,
    Named("IUw_a7") = Uw_a7+Iw_a7,
    Named("IUw_a8") = Uw_a8+Iw_a8,
    Named("IUw_a9") = Uw_a9+Iw_a9,
    Named("IUw_a10") = Uw_a10+Iw_a10,
    Named("IUw_a11") = Uw_a11+Iw_a11,
    Named("IUw_a12") = Uw_a12+Iw_a12,
    Named("IUw_a13") = Uw_a13+Iw_a13,
    Named("IUw_a14") = Uw_a14+Iw_a14,
    Named("IUw_a15") = Uw_a15+Iw_a15,
    Named("IUw_a16") = Uw_a16+Iw_a16,
    Named("Iw_a1") = Iw_a1,
    Named("Iw_a2") = Iw_a2,
    Named("Iw_a3") = Iw_a3,
    Named("Iw_a4") = Iw_a4,
    Named("Iw_a5") = Iw_a5,
    Named("Iw_a6") = Iw_a6,
    Named("Iw_a7") = Iw_a7,
    Named("Iw_a8") = Iw_a8,
    Named("Iw_a9") = Iw_a9,
    Named("Iw_a10") = Iw_a10,
    Named("Iw_a11") = Iw_a11,
    Named("Iw_a12") = Iw_a12,
    Named("Iw_a13") = Iw_a13,
    Named("Iw_a14") = Iw_a14,
    Named("Iw_a15") = Iw_a15,
    Named("Iw_a16") = Iw_a16);
  Rcpp::DataFrame byall = Rcpp::DataFrame::create(
    Named("iW")   = iW,
    Named("time") = time[iW],
    Named("Iw_g1") = Iw_g1,
    Named("Iw_g2") = Iw_g2,
    Named("Iw_g3") = Iw_g3,
    Named("Iw_g4") = Iw_g4,
    Named("Iw_g5") = Iw_g5,
    Named("Iw_g6") = Iw_g6,
    Named("Iw_g7") = Iw_g7,
    Named("Iw_g8") = Iw_g8,
    Named("Iw_g9") = Iw_g9,
    Named("Iw_g10") = Iw_g10,
    Named("Iw_g11") = Iw_g11,
    Named("Iw_g12") = Iw_g12,
    Named("Iw_g13") = Iw_g13,
    Named("Iw_g14") = Iw_g14,
    Named("Iw_g15") = Iw_g15,
    Named("Iw_g16") = Iw_g16,
    Named("Iw_g17") = Iw_g17,
    Named("Iw_g18") = Iw_g18,
    Named("Iw_g19") = Iw_g19,
    Named("Iw_g20") = Iw_g20,
    Named("Iw_g21") = Iw_g21,
    Named("Iw_g22") = Iw_g22,
    Named("Iw_g23") = Iw_g23,
    Named("Iw_g24") = Iw_g24,
    Named("Iw_g25") = Iw_g25,
    Named("Iw_g26") = Iw_g26,
    Named("Iw_g27") = Iw_g27,
    Named("Iw_g28") = Iw_g28,
    Named("Iw_g29") = Iw_g29,
    Named("Iw_g30") = Iw_g30,
    Named("Iw_g31") = Iw_g31,
    Named("Iw_g32") = Iw_g32,
    Named("Iw_g33") = Iw_g33,
    Named("Iw_g34") = Iw_g34,
    Named("Iw_g35") = Iw_g35,
    Named("Iw_g36") = Iw_g36,
    Named("Iw_g37") = Iw_g37,
    Named("Iw_g38") = Iw_g38,
    Named("Iw_g39") = Iw_g39,
    Named("Iw_g40") = Iw_g40,
    Named("Iw_g41") = Iw_g41,
    Named("Iw_g42") = Iw_g42,
    Named("Iw_g43") = Iw_g43,
    Named("Iw_g44") = Iw_g44,
    Named("Iw_g45") = Iw_g45,
    Named("Iw_g46") = Iw_g46,
    Named("Iw_g47") = Iw_g47,
    Named("Iw_g48") = Iw_g48,
    Named("Iw_g49") = Iw_g49,
    Named("Iw_g50") = Iw_g50,
    Named("Iw_g51") = Iw_g51,
    Named("Iw_g52") = Iw_g52,
    Named("Iw_g53") = Iw_g53,
    Named("Iw_g54") = Iw_g54,
    Named("Iw_g55") = Iw_g55,
    Named("Iw_g56") = Iw_g56,
    Named("Iw_g57") = Iw_g57,
    Named("Iw_g58") = Iw_g58,
    Named("Iw_g59") = Iw_g59,
    Named("Iw_g60") = Iw_g60,
    Named("Iw_g61") = Iw_g61,
    Named("Iw_g62") = Iw_g62,
    Named("Iw_g63") = Iw_g63,
    Named("Iw_g64") = Iw_g64,
    Named("Iw_g65") = Iw_g65,
    Named("Iw_g66") = Iw_g66,
    Named("Iw_g67") = Iw_g67,
    Named("Iw_g68") = Iw_g68,
    Named("Iw_g69") = Iw_g69,
    Named("Iw_g70") = Iw_g70,
    Named("Iw_g71") = Iw_g71,
    Named("Iw_g72") = Iw_g72,
    Named("Iw_g73") = Iw_g73,
    Named("Iw_g74") = Iw_g74,
    Named("Iw_g75") = Iw_g75,
    Named("Iw_g76") = Iw_g76,
    Named("Iw_g77") = Iw_g77,
    Named("Iw_g78") = Iw_g78,
    Named("Iw_g79") = Iw_g79,
    Named("Iw_g80") = Iw_g80);
  return Rcpp::List::create(Rcpp::Named("byw") = byw, Rcpp::Named("byaw") = byaw, Rcpp::Named("byall") = byall);
}
  
  
  
  