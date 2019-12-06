program GD;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  {$ENDIF}
  Interfaces, Forms, MainUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

