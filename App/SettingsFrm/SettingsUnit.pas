unit SettingsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Buttons, LCLType,
  gdPattern, gdFunctions, SimpleParameters;

type
  TSettingsFrm = class(TForm)
    btnCancel: TSpeedButton;
    btnOk: TSpeedButton;
    ColorDialog: TColorDialog;
    cbLanguage: TComboBox;
    Image1: TImage;
    lblEditorDotColor: TLabel;
    lblNewLineColor: TLabel;
    lblNewLineDotColor: TLabel;
    lblNewLineWidth: TLabel;
    lblLineWidth: TLabel;
    lblEditorDotRadius: TLabel;
    lblNewLineDotRadius: TLabel;
    lblStartPointColor: TLabel;
    lblGreedWidth: TLabel;
    lblStartPointRadius: TLabel;
    lblPatternBGColor: TLabel;
    lblGreedColor: TLabel;
    lblLineColor: TLabel;
    PaintBox: TPaintBox;
    shEditorDotColor: TShape;
    shNewLineColor: TShape;
    shNewLineDotColor: TShape;
    shStartPointColor: TShape;
    shPatternBGColor: TShape;
    shGreedColor: TShape;
    shLineColor: TShape;
    SpinNewLineWidth: TSpinEdit;
    SpinGreedWidth: TSpinEdit;
    SpinEditorDotRadius: TSpinEdit;
    SpinNewLineDotRadius: TSpinEdit;
    SpinStartPointRadius: TSpinEdit;
    spinLineWidth: TSpinEdit;

    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpinChange(Sender: TObject);
  private
    procedure VisualSettingsToInterface;
    procedure InterfaceToVisualSettings;
    procedure LoadLanguage(Lng: PSimpleParameters);
    procedure LoadLanguageBox;

    procedure PaintBmp;
  public

  end;

var
  SettingsFrm: TSettingsFrm;
  Bmp: TBitmap;
  Pattern: TgdPattern;
  VisCfg: PgdVisualSettings;
  FLngName: String;



function Execute_SettingsFrm(VisualSettings: PgdVisualSettings; Lng: PSimpleParameters; var LngName: String): Boolean;


implementation


{$R *.lfm}
{$R Pattern.res}


function Execute_SettingsFrm(VisualSettings: PgdVisualSettings; Lng: PSimpleParameters; var LngName: String): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  VisCfg := VisualSettings;
  FLngName := LngName;

  Application.CreateForm(TSettingsFrm, SettingsFrm);
  SettingsFrm.VisualSettingsToInterface;
  SettingsFrm.LoadLanguage(Lng);
  SettingsFrm.LoadLanguageBox;
  SettingsFrm.ShowModal;

  if SettingsFrm.Tag = 1 then
    begin
    SettingsFrm.InterfaceToVisualSettings;
    Result := True;

    idx := SettingsFrm.cbLanguage.ItemIndex;
    if Idx <> -1 then LngName := SettingsFrm.cbLanguage.Items.Strings[Idx];
    end;

  SettingsFrm.Free;
end;


procedure TSettingsFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: btnCancel.Click;
    VK_RETURN: btnOk.Click;
  end;
end;


procedure TSettingsFrm.PaintBoxPaint(Sender: TObject);
begin
  PaintBmp;
end;


procedure TSettingsFrm.ShapeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := TShape(Sender).Brush.Color;
  if ColorDialog.Execute then
    begin
    TShape(Sender).Brush.Color := ColorDialog.Color;
    PaintBmp;
    end;
end;


procedure TSettingsFrm.SpinChange(Sender: TObject);
begin
  PaintBmp;
end;


procedure TSettingsFrm.VisualSettingsToInterface;
begin
  shPatternBGColor.Brush.Color  := VisCfg^.PatternBGColor;
  shGreedColor.Brush.Color      := VisCfg^.GreedColor;
  SpinGreedWidth.Value          := VisCfg^.GreedWidth;
  shStartPointColor.Brush.Color := VisCfg^.StartPointColor;
  SpinStartPointRadius.Value    := VisCfg^.StartPointRadius;
  shLineColor.Brush.Color       := VisCfg^.LineColor;
  spinLineWidth.Value           := VisCfg^.LineWidth;
  shEditorDotColor.Brush.Color  := VisCfg^.EditorDotColor;
  SpinEditorDotRadius.Value     := VisCfg^.EditorDotRadius;
  shNewLineColor.Brush.Color    := VisCfg^.NewLineColor;
  SpinNewLineWidth.Value        := VisCfg^.NewLineWidth;
  shNewLineDotColor.Brush.Color := VisCfg^.NewLineDotColor;
  SpinNewLineDotRadius.Value    := VisCfg^.NewLineDotRadius;
end;


procedure TSettingsFrm.InterfaceToVisualSettings;
begin
  VisCfg^.PatternBGColor    := shPatternBGColor.Brush.Color;
  VisCfg^.GreedColor        := shGreedColor.Brush.Color;
  VisCfg^.GreedWidth        := SpinGreedWidth.Value;
  VisCfg^.StartPointColor   := shStartPointColor.Brush.Color;
  VisCfg^.StartPointRadius  := SpinStartPointRadius.Value;
  VisCfg^.LineColor         := shLineColor.Brush.Color;
  VisCfg^.LineWidth         := spinLineWidth.Value;
  VisCfg^.EditorDotColor    := shEditorDotColor.Brush.Color;
  VisCfg^.EditorDotRadius   := SpinEditorDotRadius.Value;
  VisCfg^.NewLineColor      := shNewLineColor.Brush.Color;
  VisCfg^.NewLineWidth      := SpinNewLineWidth.Value;
  VisCfg^.NewLineDotColor   := shNewLineDotColor.Brush.Color;
  VisCfg^.NewLineDotRadius  := SpinNewLineDotRadius.Value;
end;


procedure TSettingsFrm.LoadLanguage(Lng: PSimpleParameters);
var
  i, c: Integer;
  s: String;
begin
  //Загрузка меток
  c := ComponentCount - 1;
  for i := 0 to c do
    if Components[i] is TLabel then
      if SimpleParameters_Get(Lng, 'SettingsFrm.' + Components[i].Name, s) then TLabel(Components[i]).Caption := s;

  //Заголовок
  if SimpleParameters_Get(Lng, 'SettingsFrm.Caption', s) then Caption := s;
end;


procedure TSettingsFrm.LoadLanguageBox;
var
  o: TSearchRec;
  Dir: String;
begin
  Dir := ExtractFilePath(Application.ExeName) + 'Language' + PathDelim + '*.Language';
  cbLanguage.Items.BeginUpdate;


  if FindFirst(Dir, faAnyFile and not faDirectory, o) = 0 then
    cbLanguage.Items.Add(ChangeFileExt(o.Name, ''));
  while FindNext(o) = 0 do
    cbLanguage.Items.Add(ChangeFileExt(o.Name, ''));
  FindClose(o);

  cbLanguage.ItemIndex := cbLanguage.Items.IndexOf(FLngName);

  cbLanguage.Items.EndUpdate;
end;


procedure TSettingsFrm.PaintBmp;
var
  X, Y: Integer;
  pt: TPoint;
begin
  pt := Point(0, 0);

  //Очистить фон Bmp
  Bmp.Canvas.Brush.Color := shPatternBGColor.Brush.Color;
  Bmp.Canvas.Brush.Style := bsSolid;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);

  //Узор
  DrawGreed(Bmp, Pattern, shGreedColor.Brush.Color, SpinGreedWidth.Value);
  DrawPattern(Bmp, Pattern, shLineColor.Brush.Color, spinLineWidth.Value);
  DrawDots(Bmp, Pattern, shEditorDotColor.Brush.Color, SpinEditorDotRadius.Value);
  DrawStartPoint(Bmp, Pattern, shStartPointColor.Brush.Color, SpinStartPointRadius.Value);
  DrawNextLine(Bmp, Pattern, pt, shNewLineColor.Brush.Color, shNewLineDotColor.Brush.Color, SpinNewLineWidth.Value, SpinNewLineDotRadius.Value);

  //Вывод холста
  X := PaintBox.ClientWidth div 2 - Bmp.Width div 2;
  Y := PaintBox.ClientHeight div 2 - Bmp.Height div 2;
  PaintBox.Canvas.Draw(X, Y, Bmp);
end;


procedure TSettingsFrm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TSettingsFrm.btnOkClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TSettingsFrm.FormCreate(Sender: TObject);
var
  ms: TResourceStream;
  str: String;
begin
  Bmp := TBitmap.Create;
  Pattern := TgdPattern.Create('', 1, 1, Point(0, 0));

  //Загрузка из ресурсов
  ms := TResourceStream.Create(HINSTANCE, 'PATTERN', RT_RCDATA);
  SetLength(str, ms.Size);
  ms.Read(Str[1], ms.Size);
  Pattern.FromString(str);
  ms.Free;

  //Поправить битмап
  CorrectBitmapSize(PaintBox.Width, PaintBox.Height, Bmp, Pattern);
end;


procedure TSettingsFrm.FormDestroy(Sender: TObject);
begin
  Pattern.Free;
  Bmp.Free;
end;


end.

