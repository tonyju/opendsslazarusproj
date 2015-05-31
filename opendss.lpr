program opendss;

{$mode delphi}{$H+}
//{$apptype console}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,sysutils, Ucmatrix, Arraydef, Command, HashList, LineUnits, Mathutil,
  PointerList, Pstcalc, StackDef, Dynamics, RPN, ParserDel, Conductor,
  DSSGlobals, Terminal, NamedObject, Bus, pdelement, cktelementclass,
  dssclassdefs, line, transformer, vsource, utilities, dssobject, monitor,
  MeterClass, meterelement, Feeder, load, generator, solutionalgs, ymatrix,
  capcontrol, CapControlVars, ControlClass, ControlElem, GenDispatcher,
  RegControl, ExecCommands, ExecHelper, ExecOptions, Executive, ShowOptions,
  ShowResults, CableConstants, CableData, CNData, CNLineConstants,
  ConductorData, GrowthShape, LineCode, LineConstants, LineGeometry, LoadShape,
  OHLineConstants, PriceShape, Spectrum, TSData, TempShape, TSLineConstants,
  WireData, XfmrCode, Equivalent, Isource, PCClass, PCElement, Capacitor, Fault,
  Fuse, PDClass, Reactor, GeneratorVars, Sensor, LineSpacing, ReduceAlgs,circuit,CSVDocument,Ucomplex;
TYPE

  month=(jan,feb,mar,apr,may,jun,aug,sep,oct,nov,dec);
  linevalue=(line_z,line_y,line_C0,line_C1,line_CondCode,line_GeneralPlotQuantity,line_GeometryCode,line_GeometrySpecified,line_isswitch,line_KXg,line_Len,line_Lengthunits,line_LineCodeSpecified,line_NumConductorsAvailable,line_phasechoice,line_ConductorData);
  voltagesourcevalue=(vs_frombus,vs_tobus,voltagere,voltageim);
  aclinevalue=(fmutual_re,fself_re,fmutual_im,fself_im,acline_fr,acline_to);
  windingvalue=(trfmwinding_r,trfmwinding_x,trfmwinding_fr,trfmwinding_to,connectiontype,ratio);
  loadvalue=(load_re,load_im,load_fr,load_to,load_model);
  //a string parsed with ',' is used to define matrix.
  //

var

//r:real;

//s1,s2,s3,s4:string;
s1:string;
  str1 : string;

    InBlockComment : Boolean;

    cmdList: TStringList;

           imax,i,j,k,nrow,ncol,nlinecond:integer;

           inputfile:textfile;

           ParamStr:string;
           matrixstr,tmpstr,conductordatastr:string;
           dre,dim:double;
                   ndim:integer;
            circuit1:TDSSCircuit;
            line1:TLineObj;
            voltagesource:TVsourceObj;
            terminal1:TPowerTerminal;
            terminal2:TPowerTerminal;
            ADocument: TCSVDocument ;
            VBuffer: PComplexArray;
            yearend:month;
            tmpc:complex;
            tmpcond:TConductorDataObj;
            tempymatrix:TCmatrix;
            transformer1:TTransfObj;
            load1:TLoadObj;
            winding1,winding2:TWinding;
begin
    //printhello;
    //PrintHelloS(12);
//  writeln('hello!');
  writeln('hello!');
  //ord(jan):=0;
//i:=ord(jan);
//yearend:=dec;
//writeln(i);

//ord(yearend):=11;

  try
  InBlockComment := False;

  cmdList := TStringList.Create;

  cmdList.Clear;

  ExtractFilePath(ParamStr);

  DSSExecutive:=Texecutive.Create;

    if FileExists('1.txt') then

    begin
    assignfile(inputfile,'1.txt');
    reset(inputfile);
    repeat
    readln(inputfile,s1);
    str1:=Trim(s1);
    if Length(str1) > 0 then
      Begin
         if Not InBlockComment then     // look for '/*'  at baginning of line
            case str1[1] of
               '/': if (Length(str1) > 1) and (str1[2]='*')then
                    InBlockComment := TRUE;
            end;
            If Not InBlockComment Then cmdList.Add (str1);
        // in block comment ... look for */   and cancel block comment (whole line)
        if InBlockComment then
          if Pos('*/', str1)>0 then  InBlockComment := FALSE;
      End;
      {
        NOTE:  InBlockComment resets to FALSE upon leaving this routine
        So if you fail to select a line containing the end of the block comment,
        the next selection will not be blocked.
      }
    until eof(inputfile);
     closefile(inputfile);
       imax := cmdList.Count - 1;
       for i := 0 to imax do begin
           Writeln(cmdList.Strings[i]);
           DSSExecutive.Command := cmdList.Strings[i];
       end;
       //circuit1=
       ADocument:=nil;
       ADocument:=TCSVDocument.Create;
       ADocument.QuoteChar:=Char(' ');
       ADocument.Cells[ord(vs_frombus),0]:='fromnode';
       ADocument.Cells[ord(vs_tobus),0]:='tonode';
       ADocument.Cells[ord(voltagere),0]:='voltagere';
       ADocument.Cells[ord(voltageim),0]:='voltageim';
       circuit1:=Circuits.Get(1);

       for i:=1 to circuit1.Sources.ListSize do begin
               voltagesource:=circuit1.Sources.Get(i);
               circuit1.ActiveCktElement:=voltagesource;
               circuit1.ProcessBusDefs;
               //for j:=1 to voltagesource.NTerms do begin
                       terminal1:=voltagesource.Terminals^[1];
                       terminal2:=voltagesource.Terminals^[2];
                       for k:=1 to voltagesource.NConds do begin
                            str(terminal1.TermNodeRef[k],tmpstr);
                            ADocument.Cells[ord(vs_frombus),voltagesource.NConds*(i-1)+(k-1)+1]:=tmpstr;
                             str(terminal2.TermNodeRef[k],tmpstr);
                             if(tmpstr='0') then
                               begin
                                     tmpstr:='-1';
                               end;
                            ADocument.Cells[ord(vs_tobus),voltagesource.NConds*(i-1)+(k-1)+1]:=tmpstr;
                            VBuffer:=nil;
                            dre:=voltagesource.VMag*cos((voltagesource.Angle-(k-1)*120.0)*3.1415926/180.0);
                            dim:=voltagesource.VMag*sin((voltagesource.Angle-(k-1)*120.0)*3.1415926/180.0);//supose this is degree
                            //voltagesource.GetTermVoltages(i,VBuffer);
                            //tmpc:=VBuffer[k];
                            str(dre,tmpstr);
                            ADocument.Cells[ord(voltagere),voltagesource.NConds*(i-1)+(k-1)+1]:=tmpstr;
                            str(dim,tmpstr) ;
                            ADocument.Cells[ord(voltageim),voltagesource.NConds*(i-1)+(k-1)+1]:=tmpstr;
                       end;
                       //tmpstr:=voltagesource.GetBus(j);
               //end;
               //end;
       end;
       ADocument.SaveToFile('C:\opendsspowerflow\adnpf\adnpf\voltagesource.csv');

       ADocument:=nil;
       ADocument:=TCSVDocument.Create;
       ADocument.QuoteChar:=Char(' ');
       for i:=1 to circuit1.Lines.ListSize do begin
            matrixstr:='';
            line1:=circuit1.Lines.Get(i);
            str(line1.C0,tmpstr);
            ADocument.Cells[ord(line_C0),i-1]:=tmpstr;
            str(line1.C1,tmpstr);
            ADocument.Cells[ord(line_C1),i-1]:=tmpstr;
            ADocument.Cells[ord(line_CondCode),i-1]:=line1.CondCode;
            str(line1.GeneralPlotQuantity,tmpstr);
            ADocument.Cells[ord(line_GeneralPlotQuantity),i-1]:=tmpstr;
            ADocument.Cells[ord(line_GeometryCode),i-1]:=line1.GeometryCode;
            str(line1.GeometrySpecified,tmpstr);
            ADocument.Cells[ord(line_GeometrySpecified),i-1]:=tmpstr;
            str(line1.IsSwitch,tmpstr);
            ADocument.Cells[ord(line_isswitch),i-1]:=tmpstr;
            //line1.KXg:=;
            str(line1.KXg,tmpstr);
            ADocument.Cells[ord(line_KXg),i-1]:=tmpstr;
            str(line1.Len,tmpstr);
            ADocument.Cells[ord(line_Len),i-1]:=tmpstr;
            str(line1.LengthUnits,tmpstr);
            ADocument.Cells[ord(line_Lengthunits),i-1]:=tmpstr;
            str(line1.LineCodeSpecified,tmpstr);
            ADocument.Cells[ord(line_LineCodeSpecified),i-1]:=tmpstr;
            str(line1.NumConductorsAvailable,tmpstr);
            ADocument.Cells[ord(line_NumConductorsAvailable),i-1]:=tmpstr;
            str(ord(line1.PhaseChoice),tmpstr);
            ADocument.Cells[ord(line_phasechoice),i-1]:=tmpstr;
            conductordatastr:='';
            str(line1.NumConductorsAvailable,tmpstr);
            conductordatastr:=conductordatastr+tmpstr;
            for nlinecond:=1 to line1.NumConductorsAvailable do begin
                 tmpcond:=line1.ConductorData[nlinecond];
                 str(tmpcond.EmergAmps,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                 str(tmpcond.GMR,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                 str(tmpcond.GMRUnits,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.NormAmps,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                   str(tmpcond.Rac,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.Rdc,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.ResUnits,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.Radius,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.RadiusUnits,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.ClassIndex,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.DSSObjType,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.HasBeenSaved,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                                  str(tmpcond.Flag,tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                 str(line1.lineGeometryobj.Xcoord[nlinecond],tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                 str(line1.lineGeometryobj.Ycoord[nlinecond],tmpstr);
                 conductordatastr:=conductordatastr+';'+tmpstr;
                 //str(line1.lineGeometryobj.EmergAmps,str);
                 //                 str(tmpcond.,tmpstr);
                 //conductordatastr:=conductordatastr+';'+tmpstr;

                 //if tmpcond is  then
            end;
            ADocument.Cells[ord(line_ConductorData),i-1]:=conductordatastr;
            ndim:=line1.Z.Order;
            Str(ndim, tmpstr);
            matrixstr:=matrixstr+tmpstr;
            for nrow:=1 to  ndim do
            begin
                 for ncol:=1 to ndim do
                 begin
                 tmpc:=line1.Z.GetElement(nrow,ncol);
                 dre:=tmpc.re;
                 dim:=tmpc.im;
                 matrixstr:=matrixstr+';'+'(';
                 str(dre,tmpstr);
                 matrixstr:=matrixstr+tmpstr;
                 matrixstr:=matrixstr+' ';
                 str(dim,tmpstr);
                 matrixstr:=matrixstr+tmpstr+')';
                 end;
            end;
            //line1.ToTerminal:=;
            line1.CalcYPrim();
            //ADocument.Cells[ord(line_z),i-1]:=matrixstr;
            line1.GetYPrim(tempYmatrix,ALL_YPRIM);
            //matrixstr:=matrixstr+1;
            matrixstr:='';
            for nrow:=1 to tempYmatrix.Order do
            begin
                for ncol:=1 to tempYmatrix.Order do begin
                    tmpc:=tempYmatrix.GetElement(nrow,ncol);
                    dre:=tmpc.re;
                    dim:=tmpc.im;
                    matrixstr:=matrixstr+';'+'(';
                    str(dre,tmpstr);
                    matrixstr:=matrixstr+tmpstr;
                    matrixstr:=matrixstr+' ';
                    str(dim,tmpstr);
                    matrixstr:=matrixstr+tmpstr+')';
                end;
            end;
            ADocument.Cells[ord(line_y),i-1]:=matrixstr;
            //line1.ConductorData[i];
            //ADocument.LoadFromStream();
       //Writeln(Get(1));
       end;
       ADocument.SaveToFile('2.csv');

       //acline
       ADocument:=nil;
       ADocument:=TCSVDocument.Create;
       ADocument.QuoteChar:=Char(' ');
       ADocument.Cells[ord(acline_fr),0]:='fromnode';
       ADocument.Cells[ord(acline_to),0]:='tonode';
       ADocument.Cells[ord(fmutual_re),0]:='fmutual_re';
       ADocument.Cells[ord(fself_re),0]:='fself_re';
       ADocument.Cells[ord(fmutual_im),0]:='fmutual_im';
       ADocument.Cells[ord(fself_im),0]:='fself_im';
       for i:=1 to circuit1.Lines.ListSize do begin
            matrixstr:='';
            line1:=circuit1.Lines.Get(i);
            circuit1.ActiveCktElement:=line1;
            circuit1.ProcessBusDefs;
            //for j:=1 to voltagesource.NTerms do begin
                    terminal1:=line1.Terminals^[1];
                    terminal2:=line1.Terminals^[2];
                    for k:=1 to line1.NConds do begin
                         str(terminal1.TermNodeRef[k],tmpstr);
                         ADocument.Cells[ord(acline_fr),i-1+1]:=ADocument.Cells[ord(acline_fr),i-1+1]+';'+tmpstr;
                          str(terminal2.TermNodeRef[k],tmpstr);
                         ADocument.Cells[ord(acline_to),i-1+1]:=ADocument.Cells[ord(acline_to),i-1+1]+';'+tmpstr;
                    end;
            matrixstr:='';
            Str(line1.Z.GetElement(1,1).re, tmpstr);
            matrixstr:=matrixstr+tmpstr;
            Str(line1.Z.GetElement(2,2).re, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            Str(line1.Z.GetElement(3,3).re, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            ADocument.Cells[ord(fself_re),i-1+1]:=matrixstr;
            matrixstr:='';
            Str(line1.Z.GetElement(1,1).im, tmpstr);
            matrixstr:=matrixstr+tmpstr;
            Str(line1.Z.GetElement(2,2).im, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            Str(line1.Z.GetElement(3,3).im, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            ADocument.Cells[ord(fself_im),i-1+1]:=matrixstr;
            matrixstr:='';
            Str(line1.Z.GetElement(1,2).re, tmpstr);
            matrixstr:=matrixstr+tmpstr;
            Str(line1.Z.GetElement(1,3).re, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            Str(line1.Z.GetElement(2,3).re, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            ADocument.Cells[ord(fmutual_re),i-1+1]:=matrixstr;
            matrixstr:='';
            Str(line1.Z.GetElement(1,2).im, tmpstr);
            matrixstr:=matrixstr+tmpstr;
            Str(line1.Z.GetElement(1,3).im, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            Str(line1.Z.GetElement(2,3).im, tmpstr);
            matrixstr:=matrixstr+';'+tmpstr;
            ADocument.Cells[ord(fmutual_im),i-1+1]:=matrixstr;
            //line1.ConductorData[i];
            //ADocument.LoadFromStream();
       //Writeln(Get(1));
       end;
       ADocument.SaveToFile('C:\opendsspowerflow\adnpf\adnpf\acline.csv');


       //transformer winding
       ADocument:=nil;
       ADocument:=TCSVDocument.Create;
       ADocument.QuoteChar:=Char(' ');
       for i:=1 to circuit1.Transformers.ListSize do begin
            matrixstr:='';
            transformer1:=circuit1.Transformers.Get(i);
            circuit1.ActiveCktElement:=transformer1;
            circuit1.ProcessBusDefs;
       ADocument.Cells[ord(trfmwinding_fr),0]:='fromnode';
       ADocument.Cells[ord(trfmwinding_to),0]:='tonode';
       ADocument.Cells[ord(connectiontype),0]:='connectiontype';
       ADocument.Cells[ord(trfmwinding_r),0]:='trfmwinding_r';
       ADocument.Cells[ord(trfmwinding_x),0]:='trfmwinding_x';
       ADocument.Cells[ord(ratio),0]:='ratio';
              if transformer1.NumberOfWindings =2 then begin
                   winding1:=transformer1.Winding[1];
                   winding2:=transformer1.Winding[2];
                   terminal1:=transformer1.Terminals^[1];
                   terminal2:=transformer1.Terminals^[2];
                    for k:=1 to transformer1.NPhases do begin
                         if  terminal1.TermNodeRef[k]=0 then
                         continue;
                         //end;
                         str(terminal1.TermNodeRef[k],tmpstr);
                         ADocument.Cells[ord(trfmwinding_fr),i-1+1]:=ADocument.Cells[ord(trfmwinding_fr),i-1+1]+';'+tmpstr;
                          str(terminal2.TermNodeRef[k],tmpstr);
                         ADocument.Cells[ord(trfmwinding_to),i-1+1]:=ADocument.Cells[ord(trfmwinding_to),i-1+1]+';'+tmpstr;
                         str(transformer1.WdgResistance[1],tmpstr);
                         ADocument.Cells[ord(trfmwinding_r),i-1+1]:=ADocument.Cells[ord(trfmwinding_r),i-1+1]+';'+tmpstr;
                         str(transformer1.WdgResistance[2],tmpstr);
                         ADocument.Cells[ord(trfmwinding_r),i-1+1]:=ADocument.Cells[ord(trfmwinding_r),i-1+1]+';'+tmpstr;
                         str(transformer1.XscVal[1],tmpstr);
                         ADocument.Cells[ord(trfmwinding_x),i-1+1]:=ADocument.Cells[ord(trfmwinding_x),i-1+1]+';'+tmpstr;
                         str(transformer1.XscVal[2],tmpstr);
                         ADocument.Cells[ord(trfmwinding_x),i-1+1]:=ADocument.Cells[ord(trfmwinding_x),i-1+1]+';'+tmpstr;
                    end;
                         str(winding1.Connection,tmpstr);
                         ADocument.Cells[ord(connectiontype),i-1+1]:=tmpstr;
                         str(winding2.Connection,tmpstr);//  {     0 = line-neutral; 1=Delta}
                         ADocument.Cells[ord(connectiontype),i-1+1]:=ADocument.Cells[ord(connectiontype),i-1+1]+';'+tmpstr;
                         str(transformer1.BaseVoltage[1]/ transformer1.BaseVoltage[2],tmpstr);
                         ADocument.Cells[ord(ratio),i-1+1]:=tmpstr;

              end;
            //circuit1.ActiveCktElement:=transformer1;
            //circuit1.ProcessBusDefs;

            //for j:=1 to voltagesource.NTerms do begin

       end;
       ADocument.SaveToFile('C:\opendsspowerflow\adnpf\adnpf\trfmwinding.csv');

       //load energy consumer
       ADocument:=nil;
       ADocument:=TCSVDocument.Create;
       ADocument.QuoteChar:=Char(' ');
       ADocument.Cells[ord(load_fr),0]:='fromnode';
       ADocument.Cells[ord(load_to),0]:='tonode';
       ADocument.Cells[ord(connectiontype),0]:='connectiontype';
       ADocument.Cells[ord(load_re),0]:='loadre';
       ADocument.Cells[ord(load_im),0]:='loadim';

       for i:=1 to circuit1.Loads.ListSize do begin
            matrixstr:='';
            load1:=circuit1.Loads.Get(i);
            circuit1.ActiveCktElement:=load1;
            circuit1.ProcessBusDefs;

              for j:=1 to load1.NConds do begin

                   if load1.Connection=0 then
                   begin{     0 = line-neutral; 1=Delta}
                   terminal1:=load1.Terminals^[1];
                   end
                   else begin
                   terminal1:=load1.Terminals^[1];
                   terminal2:=load1.Terminals^[2];
                   end;
                    for k:=1 to load1.NPhases do begin
                         //end;
                         str(terminal1.TermNodeRef[k],tmpstr);
                         if      terminal1.TermNodeRef[k]=0 then
                         continue;
                         if load1.Connection=0 then
                         begin
                         ADocument.Cells[ord(load_fr),load1.NPhases*(i-1)+k-1+1]:=tmpstr;
                         ADocument.Cells[ord(load_to),load1.NPhases*(i-1)+k-1+1]:='-1';
                         end
                         else
                         begin
                          ADocument.Cells[ord(load_fr),load1.NPhases*(i-1)+k-1+1]:=tmpstr;
                         str(terminal2.TermNodeRef[k],tmpstr);
                         ADocument.Cells[ord(load_to),load1.NPhases*(i-1)+k-1+1]:=tmpstr;
                         end;
                         // Variation with voltage
          {  1 = Constant kVA (P,Q always in same ratio)
             2 = Constant impedance
             3 = Constant P, Quadratic Q (Mostly motor)
             4 = Linear P, Quadratic Q  (Mixed motor/resistive Use this for CVR studies
             5 = Constant |I|
             6 = Constant P (Variable); Q is fixed value (not variable)
             7 = Constant P (Variable); Q is fixed Z (not variable)
             8 = ZIPV (3 real power coefficients, 3 reactive, Vcutoff)
          }
                      str(load1.FLoadModel,tmpstr);
                     ADocument.Cells[ord(load_model),load1.NConds*(i-1)+k-1+1]:=tmpstr;
                         if   load1.FLoadModel=1 then
                         begin
                         str(load1.WNominal,tmpstr);
                         ADocument.Cells[ord(load_re),load1.NConds*(i-1)+k-1+1]:=tmpstr;
                         str(load1.varNominal,tmpstr);
                         ADocument.Cells[ord(load_im),load1.NConds*(i-1)+k-1+1]:=tmpstr;
                         end;
                    end;
              end;
            //circuit1.ActiveCktElement:=transformer1;
            //circuit1.ProcessBusDefs;

            //for j:=1 to voltagesource.NTerms do begin

       end;
       ADocument.SaveToFile('C:\opendsspowerflow\adnpf\adnpf\load.csv');



     cmdList.Free;
    end;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
end.


