unit SizeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons,
  Spin, LCLType, ExtCtrls,
  SimpleParameters;

type
  TSizeFrm = class(TForm)
    btn50: TSpeedButton;
    btn60: TSpeedButton;
    btn70: TSpeedButton;
    btn80: TSpeedButton;
    btnCancel: TSpeedButton;
    btnOk: TSpeedButton;
    Image1: TImage;
    Image2: TImage;
    btn10: TSpeedButton;
    btn20: TSpeedButton;
    btn30: TSpeedButton;
    btn40: TSpeedButton;
    SpinWidth: TSpinEdit;
    SpinHeight: TSpinEdit;
    procedure ButtonClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

    procedure LoadLanguage(Lng: PSimpleParameters);
  public

  end;


var
  SizeFrm: TSizeFrm;




function Execute_SizeFrm(var Width, Height: Integer; Lng: PSimpleParameters): Boolean;



implementation

{$R *.lfm}



function Execute_SizeFrm(var Width, Height: Integer; Lng: PSimpleParameters): Boolean;
begin
  Result := False;

  Application.CreateForm(TSizeFrm, SizeFrm);

  SizeFrm.SpinWidth.Value := Width;
  SizeFrm.SpinHeight.Value := Height;
  SizeFrm.LoadLanguage(Lng);

  SizeFrm.ShowModal;

  if SizeFrm.Tag = 1 then
    begin
    Width := SizeFrm.SpinWidth.Value;
    Height := SizeFrm.SpinHeight.Value;
    Result := True;
    end;

  SizeFrm.Free;

end;


procedure TSizeFrm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TSizeFrm.ButtonClick(Sender: TObject);
begin
  SpinWidth.Value := TSpeedButton(Sender).Tag;
  SpinHeight.Value := TSpeedButton(Sender).Tag;
end;


procedure TSizeFrm.btnOkClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TSizeFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: btnCancel.Click;
    VK_RETURN: btnOk.Click;
  end;
end;


procedure TSizeFrm.LoadLanguage(Lng: PSimpleParameters);
var
  s: String;
begin
  if SimpleParameters_Get(Lng, 'SizeFrm.Caption', s) then Caption := s;
  if SimpleParameters_Get(Lng, 'SizeFrm.BtnOk', s) then btnOk.Caption := s;
  if SimpleParameters_Get(Lng, 'SizeFrm.BtnCancel', s) then btnCancel.Caption := s;
end;



end.


