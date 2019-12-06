unit AboutUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  SimpleParameters, LCLType, StdCtrls, ExtCtrls;


type
  TAboutFrm = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure LoadLanguage(Lng: PSimpleParameters);
  public

  end;


var
  AboutFrm: TAboutFrm;


procedure Execute_AboutFrm(Lng: PSimpleParameters);


implementation

{$R *.lfm}



procedure Execute_AboutFrm(Lng: PSimpleParameters);
begin
  Application.CreateForm(TAboutFrm, AboutFrm);
  AboutFrm.LoadLanguage(Lng);
  AboutFrm.ShowModal;
  AboutFrm.Free;
end;


procedure TAboutFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Close;
end;


procedure TAboutFrm.LoadLanguage(Lng: PSimpleParameters);
var
  s: String;
begin
  if SimpleParameters_Get(Lng, 'AboutFrm.Caption', s) then Caption := s;
end;



end.

