unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, ExtDlgs, Types, SizeUnit, SettingsUnit, AboutUnit,
  StringArray, SimpleParameters, gdPattern, gdFunctions;


const
  VERSION = 'v0.5';


type
  TMainFrm = class(TForm)
    MainMenu: TMainMenu;
    mClear: TMenuItem;
    mAbout: TMenuItem;
    mEditMode: TMenuItem;
    mTask3: TMenuItem;
    mTask2: TMenuItem;
    mTask1: TMenuItem;
    mSeparator4: TMenuItem;
    mSeparator2: TMenuItem;
    mExportTask: TMenuItem;
    mSettings: TMenuItem;
    mTools: TMenuItem;
    mFitSize: TMenuItem;
    mSize: TMenuItem;
    mSeparator3: TMenuItem;
    mImage: TMenuItem;
    mFile: TMenuItem;
    mSeparator1: TMenuItem;
    mClose: TMenuItem;
    mSaveAs: TMenuItem;
    mOpen: TMenuItem;
    mSave: TMenuItem;
    OpenDialog: TOpenDialog;
    PaintBox: TPaintBox;
    SaveDialog: TSaveDialog;
    SavePictureDialog: TSavePictureDialog;
    StatusBar: TStatusBar;

    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mAboutClick(Sender: TObject);
    procedure mClearClick(Sender: TObject);
    procedure mCloseClick(Sender: TObject);
    procedure mEditModeClick(Sender: TObject);
    procedure mFitSizeClick(Sender: TObject);
    procedure mOpenClick(Sender: TObject);
    procedure mSaveAsClick(Sender: TObject);
    procedure mSaveClick(Sender: TObject);
    procedure mSettingsClick(Sender: TObject);
    procedure mSizeClick(Sender: TObject);
    procedure mTask1Click(Sender: TObject);
    procedure mTask2Click(Sender: TObject);
    procedure mTask3Click(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);


    procedure SaveTask(Pattern: Boolean = True; Task: Boolean = True);
    function  GetWorkArea(Bmp: TBitmap; Pattern: TgdPattern): TRect;
    function  GetCellCoordinate(Bmp: TBitmap; Pattern: TgdPattern; X, Y: Integer): TPoint;
    procedure CorrectInterface;
    procedure PaintCanvas;
    procedure LoadLanguage(FileName: String);
    procedure ApplyLanguage;
    procedure PrepareSettings;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ApplySettings;
    function  GetStrValueDefault(sName: String; DefValue: String): String;
    function  GetIntValueDefault(sName: String; DefValue: Integer): Integer;
    function  GetColValueDefault(sName: String; DefValue: TColor): TColor;
    procedure SplitterChangeBounds(Sender: TObject);

    procedure SetEditorMode(AMode: Boolean);
  private
    FLngDir: String;
    FBmp: TBitmap;
    FPattern: TgdPattern;
    FSettings: TSimpleParameters;
    FVisCfg: TgdVisualSettings;
    FLanguage: TSimpleParameters;
    FLanguageName: String;

    FLngCaption: String;
    FLngDialogCaption: String;
    FLngDialogInfo: String;
    FLngNewDocName: String;
    FLngSaveFileError: String;
    FLngStartWord: String;
    FLngActionWord: String;


    //Интерфейс
    FLastPoint: TPoint;         //Координата клетки после последнего движения мыши
    FMoveStartPoint: Boolean;   //Шевелим стартовую точку
    FMovePoint: Boolean;        //Шевелим обычную точку
    FMovePointIdx: Integer;     //Индекс точки для изменения
    FEditMode: Boolean;         //Режим редактирования
    FWorkMode: Byte;            //Тип режима работы редактора
    FModified: Boolean;         //Флаг изменения
    FLastFile: String;          //Имя файла
  public

    property EditMode: Boolean read FEditMode write SetEditorMode;
  end;

var
  MainFrm: TMainFrm;


implementation

{$R *.lfm}


const
  ModeEdit = 0;             //Добавление/Удаление точек
  ModeMovePoint = 1;        //Двигать точку
  ModeMoveStartPoint = 2;   //Двигать стартовую точку




procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FPattern.Free;
  FBmp.Free;

  SaveSettings;
  SimpleParameters_Clear(@FSettings);
end;


procedure TMainFrm.FormResize(Sender: TObject);
begin
  CorrectBitmapSize(PaintBox.Width, PaintBox.Height, FBmp, FPattern);
end;


procedure TMainFrm.mAboutClick(Sender: TObject);
begin
  Execute_AboutFrm(@FLanguage);
end;


procedure TMainFrm.mClearClick(Sender: TObject);
begin
  FPattern.ClearPoints;
  FPattern.ResetStartPoint;

  FModified := True;
  CorrectInterface;
  PaintCanvas;
end;


procedure TMainFrm.mCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMainFrm.mEditModeClick(Sender: TObject);
begin
  EditMode := not EditMode;
end;


procedure TMainFrm.mFitSizeClick(Sender: TObject);
begin
  FPattern.FitSize;

  CorrectBitmapSize(PaintBox.Width, PaintBox.Height, FBmp, FPattern);
  FLastPoint := Point(0, 0);
  CorrectInterface;
  PaintBox.Repaint;
  PaintCanvas;
end;


procedure TMainFrm.mOpenClick(Sender: TObject);
begin
  if FModified then
    if MessageDlg(FLngCaption, FLngDialogInfo + #13#10'"' + FPattern.Name + '"?', mtConfirmation, mbYesNo, 0) = mrYes then mSave.Click;

  if OpenDialog.Execute then
    begin
    FPattern.LoadFromFile(OpenDialog.FileName);
    FEditMode := False;
    FModified := False;
    FLastFile := OpenDialog.FileName;
    CorrectBitmapSize(PaintBox.Width, PaintBox.Height, FBmp, FPattern);
    CorrectInterface;
    PaintBox.Repaint;
    PaintCanvas;
    end;
end;


procedure TMainFrm.mSaveAsClick(Sender: TObject);
var
  fn: String;
begin
  if FLastFile = '' then fn := FormatDateTime('yyyy.mm.dd-hh.nn.ss', Now) else fn := ChangeFileExt(ExtractFileName(FLastFile), '');
  SaveDialog.FileName := fn;

  if SaveDialog.Execute then
    begin
    fn := ChangeFileExt(SaveDialog.FileName, '.gd');
    FPattern.SaveToFile(fn);
    FLastFile := SaveDialog.FileName;
    FModified := False;
    CorrectInterface;
    end;
end;


procedure TMainFrm.mSaveClick(Sender: TObject);
var
  fn: String;
begin
  fn := '';
  if FLastFile = '' then
    begin
    SaveDialog.FileName := FormatDateTime('yyyy.mm.dd-hh.nn.ss', Now);
    if SaveDialog.Execute then fn := SaveDialog.FileName;
    end else fn := FLastFile;

  if fn <> '' then
    begin
    fn := ChangeFileExt(fn, '.gd');
    FPattern.SaveToFile(fn);
    FLastFile := fn;
    FModified := False;
    CorrectInterface;
    end;
end;


procedure TMainFrm.mSettingsClick(Sender: TObject);
var
  VisualSettings: TgdVisualSettings;
  LngName: String;
begin
  VisualSettings := FVisCfg;
  LngName := FLanguageName;


  if Execute_SettingsFrm(@VisualSettings, @FLanguage, LngName) then
    begin
    FVisCfg := VisualSettings;
    PaintCanvas;

    if LngName <> FLanguageName then
      begin
      FLanguageName := LngName;
      LoadLanguage(FLngDir + FLanguageName + '.Language');
      ApplyLanguage;
      end;


    end;
end;


procedure TMainFrm.mSizeClick(Sender: TObject);
var
  W, H: Integer;
begin
  W := FPattern.Width;
  H := FPattern.Height;

  if Execute_SizeFrm(W, H, @FLanguage) then
    begin
    FPattern.Width := W;
    FPattern.Height := H;

    CorrectBitmapSize(PaintBox.Width, PaintBox.Height, FBmp, FPattern);
    CorrectInterface;
    PaintBox.Repaint;
    PaintCanvas;
    end;
end;


procedure TMainFrm.mTask1Click(Sender: TObject);
begin
  SaveTask(True, True);
end;


procedure TMainFrm.mTask2Click(Sender: TObject);
begin
  SaveTask(True, False);
end;


procedure TMainFrm.mTask3Click(Sender: TObject);
begin
  SaveTask(False, True);
end;


procedure TMainFrm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  idx: Integer;
begin
  //Запомнить последние координаты точки
  FLastPoint := GetCellCoordinate(FBmp, FPattern, X, Y);

  //Переключить режим редактирования
  if Button = mbMiddle then EditMode := not EditMode;

  //Выход если не режим редактора
  if not FEditMode then Exit;




  //Определить режим работы редактора
  FWorkMode := ModeEdit;

  idx := FPattern.GetPointIndexByPos(FLastPoint);
  if (FWorkMode = ModeEdit) and (Button = mbRight) and (idx <> -1) then
    begin
    FWorkMode := ModeMovePoint;
    FMovePointIdx := idx;
    end;

  if (FWorkMode = ModeEdit) and (Button = mbRight) and IsPointEqual(FLastPoint, FPattern.StartPoint) then FWorkMode := ModeMoveStartPoint;


  //Обработать режим
  case FWorkMode of
    ModeEdit:
      begin
      //Удалить точку
      if Button = mbRight then
        begin
        FPattern.DeleteLastPoint;
        PaintCanvas;
        FModified := True;
        CorrectInterface;
        end;

      //Добавить точку
      if (Button = mbLeft) and (FLastPoint.X <> -1) and (FLastPoint.Y <> -1) then
        begin
        pt := FPattern.GetLastPoint;
        if not IsPointEqual(FLastPoint, pt) then
          begin
          FPattern.AddPoint(FLastPoint.X - pt.X, FLastPoint.Y - pt.Y);
          PaintCanvas;
          FModified := True;
          CorrectInterface;
          end;
        end;
      end;

    ModeMovePoint:
      FMovePoint := True;

    ModeMoveStartPoint:
      FMoveStartPoint := True;
  end;
end;


procedure TMainFrm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pt, pt2: TPoint;
begin
  //Запомнить последние координаты точки
  FLastPoint := GetCellCoordinate(FBmp, FPattern, X, Y);



  if not FEditMode then
    begin
    CorrectInterface;
    Exit;
    end;


  //Поправить курсор
  if IsPointEqual(FLastPoint, FPattern.StartPoint) or (FPattern.GetPointIndexByPos(FLastPoint) <> -1) then
    PaintBox.Cursor := crSizeAll else PaintBox.Cursor := crArrow;


  //Обработать режим
  case FWorkMode of
    ModeEdit:
      begin

      PaintCanvas;
      end;


    ModeMovePoint:
      begin
      if (FLastPoint.X <> -1) and (FLastPoint.Y <> -1) then
        begin
        pt := FPattern.GetPointByIdx(FMovePointIdx);
        Dec(pt.X, FLastPoint.X);
        Dec(pt.Y, FLastPoint.Y);
        pt2 := FPattern.RawPoint[FMovePointIdx];

        pt.X := pt2.X - pt.X;
        pt.Y := pt2.Y - pt.Y;

        FPattern.RawPoint[FMovePointIdx] := pt;

        PaintCanvas;
        FModified := True;
        end;
      end;


    ModeMoveStartPoint:
      begin
      //Шевелить точку
      if (FLastPoint.X <> -1) and (FLastPoint.Y <> -1) then
        begin
        FPattern.StartPoint := FLastPoint;
        FModified := True;
        PaintCanvas;
        end;
      end;
  end;


  CorrectInterface;
end;


procedure TMainFrm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FEditMode then Exit;

  case FWorkMode of
    ModeEdit:
      begin

      end;

    ModeMoveStartPoint:
      begin
      FWorkMode := ModeEdit;
      if Button = mbLeft then FMoveStartPoint := False;
      end;

    ModeMovePoint:
      begin
      FWorkMode := ModeEdit;
      if Button = mbLeft then FMovePoint := False;
      end;
  end;

  PaintCanvas;
end;


procedure TMainFrm.PaintBoxPaint(Sender: TObject);
begin
  PaintCanvas;
end;


procedure TMainFrm.SaveTask(Pattern: Boolean; Task: Boolean);
var
  ExportSettings: TExportTaskConfig;
  VisualSettings: TgdVisualSettings;
  Bmp: TBitmap;
  jp: TJPEGImage;
  fn: String;
begin
  SavePictureDialog.FileName := FPattern.Name;

  if SavePictureDialog.Execute then
    begin
    ExportSettings := GetDefaultExportTaskSettings;
    ExportSettings.VisiblePattern := Pattern;
    ExportSettings.VisibleTask := Task;

    VisualSettings := FVisCfg;
    VisualSettings.LineWidth := VisualSettings.LineWidth * 2;
    VisualSettings.StartPointRadius := VisualSettings.StartPointRadius * 3;

    Bmp := TBitmap.Create;
    PrepareExportBmp(Bmp, ExportSettings, VisualSettings, FPattern, FLngStartWord, FLngActionWord);

    jp := TJPEGImage.Create;
    jp.CompressionQuality := 100;
    jp.Width := Bmp.Width;
    jp.Height := Bmp.Height;

    try
      try
        case SavePictureDialog.FilterIndex of
          1:  begin
              fn := ChangeFileExt(SavePictureDialog.FileName, '.jpg');
              jp.Canvas.Draw(0, 0, Bmp);
              jp.SaveToFile(fn);
              end;
          2:  begin
              fn := ChangeFileExt(SavePictureDialog.FileName, '.bmp');
              Bmp.SaveToFile(fn);
              end;
        end;
      except
        raise Exception.Create(FLngSaveFileError + ' "' + SavePictureDialog.FileName + '"');
      end;

    finally
      jp.Free;
      Bmp.Free;
    end;
    end;
end;


function TMainFrm.GetWorkArea(Bmp: TBitmap; Pattern: TgdPattern): TRect;
var
  CellSize: TgdFloatPoint;
begin
  //Размер клетки
  CellSize := GetCellSize(Bmp, Pattern);

  //Координаты области вывода
  Result.Left := PaintBox.ClientWidth div 2 - FBmp.Width div 2 + Round(CellSize.X);
  Result.Top := PaintBox.ClientHeight div 2 - FBmp.Height div 2 + Round(CellSize.Y);
  Result.Right := Result.Left + FBmp.Width - Round(CellSize.X * 2);
  Result.Bottom := Result.Top + FBmp.Height - Round(CellSize.Y * 2);
end;


function TMainFrm.GetCellCoordinate(Bmp: TBitmap; Pattern: TgdPattern; X, Y: Integer): TPoint;
var
  Rct: TRect;
  CellSize: TgdFloatPoint;
begin
  Result := Point(-1, -1);

  Rct := GetWorkArea(Bmp, Pattern);
  CellSize := GetCellSize(FBmp, FPattern);

  if PtInRect(Rct, Point(X, Y)) then
    begin
    Result.X := Round(Round((X - Rct.Left) / CellSize.X));
    Result.Y := Round(Round((Y - Rct.Top) / CellSize.Y));
    end;
end;


procedure TMainFrm.CorrectInterface;
begin
  Caption := FPattern.Name + ' - ' + FLngCaption + ' ' + VERSION;

  mSave.Enabled := FModified;

  StatusBar.Panels[1].Text := IntToStr(FPattern.Width) + ' x ' + IntToStr(FPattern.Height);
  StatusBar.Panels[3].Text := IntToStr(FPattern.PointCount);
  StatusBar.Panels[5].Text := IntToStr(FLastPoint.X) + ' x ' + IntToStr(FLastPoint.Y);
end;


procedure TMainFrm.FormCreate(Sender: TObject);
begin
  //Холст для рисования
  FBmp := TBitmap.Create;
  FBmp.Canvas.Brush.Color := clWhite;


  //Настройки отображения
  with FVisCfg do
    begin
    PatternBGColor := clWhite;
    GreedColor := RGBToColor(200, 200, 200);
    GreedWidth := 1;
    StartPointColor := clRed;
    StartPointRadius := 7;
    LineColor := clBlack;
    LineWidth := 5;
    EditorDotColor := clBlue;
    EditorDotRadius := 5;
    NewLineColor := clRed;
    NewLineWidth := 5;
    NewLineDotColor := clRed;
    NewLineDotRadius := 5;
    end;

  //Остальная шляпа
  FLngDir := ExtractFilePath(Application.ExeName) + 'Language' + PathDelim;
  FModified := False;
  FLastFile := '';
  FMovePoint := False;
  FMovePointIdx := -1;
  FWorkMode := ModeEdit;

  PrepareSettings;
  LoadSettings;
  ApplySettings;


  FLngCaption := 'Graphic Dictation';
  FLngDialogCaption := 'What to do...';
  FLngDialogInfo := 'Save changes to the pattern';
  FLngSaveFileError := 'Save file error';
  FLngNewDocName := 'New';
  FLngStartWord := 'Start:';
  FLngActionWord := 'Actions:';


  LoadLanguage(FLngDir + FLanguageName + '.Language');
  ApplyLanguage;

  //Узор
  FPattern := TgdPattern.Create(FLngNewDocName, 20, 20, Point(0, 0));


  SetEditorMode(False);
  CorrectInterface;
end;


procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  r: TModalResult;
begin
  if FModified then
    begin
    r := MessageDlg(FLngCaption, FLngDialogInfo + #13#10'"' + FPattern.Name + '"?', mtConfirmation, mbYesNoCancel, 0);
    case r of
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
      mrYes: mSave.Click;
    end;
    end;
end;


procedure TMainFrm.PaintCanvas;
var
  X, Y: Integer;
begin
  //Очистить фон Bmp
  FBmp.Canvas.Brush.Color := FVisCfg.PatternBGColor;
  FBmp.Canvas.Brush.Style := bsSolid;
  FBmp.Canvas.FillRect(0, 0, FBmp.Width, FBmp.Height);

  //Нарисовать узор
  DrawGreed(FBmp, FPattern, FVisCfg.GreedColor, FVisCfg.GreedWidth);
  DrawPattern(FBmp, FPattern, FVisCfg.LineColor, FVisCfg.LineWidth);
  if FEditMode then DrawDots(FBmp, FPattern, FVisCfg.EditorDotColor, FVisCfg.EditorDotRadius);
  DrawStartPoint(FBmp, FPattern, FVisCfg.StartPointColor, FVisCfg.StartPointRadius);
  if FEditMode then DrawNextLine(FBmp, FPattern, FLastPoint, FVisCfg.NewLineColor, FVisCfg.NewLineDotColor, FVisCfg.NewLineWidth, FVisCfg.NewLineDotRadius);

  //Вывод холста
  X := PaintBox.ClientWidth div 2 - FBmp.Width div 2;
  Y := PaintBox.ClientHeight div 2 - FBmp.Height div 2;
  PaintBox.Canvas.Draw(X, Y, FBmp);
end;


procedure TMainFrm.LoadLanguage(FileName: String);
begin
  SimpleParameters_LoadFromFile(@FLanguage, FileName);
end;


procedure TMainFrm.ApplyLanguage;
var
  i, c: Integer;
  s: String;
begin
  //Загрузка меню
  c := ComponentCount - 1;
  for i := 0 to c do
    if Components[i] is TMenuItem then
      if SimpleParameters_Get(@FLanguage, 'MainMenu.' + Components[i].Name, s) then TMenuItem(Components[i]).Caption := s;

  //Заголовок
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Caption', s) then FLngCaption := s;
  Application.Title := FLngCaption;

  //Имя нового документа
  if SimpleParameters_Get(@FLanguage, 'MainFrm.NewPatternName', s) then FLngNewDocName := s;

  //Не могу сохранить файл
  if SimpleParameters_Get(@FLanguage, 'MainFrm.SaveFileError', s) then FLngSaveFileError := s;

  //Панель
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Panel.Size', s) then StatusBar.Panels[0].Text := s;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Panel.Action', s) then StatusBar.Panels[2].Text := s;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Panel.Coordinate', s) then StatusBar.Panels[4].Text := s;

  //Диалоги
  if SimpleParameters_Get(@FLanguage, 'MainFrm.ExportDialog.Title', s) then SavePictureDialog.Title := s;

  if SimpleParameters_Get(@FLanguage, 'MainFrm.OpenDialog.Title', s) then OpenDialog.Title := s;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.SaveDialog.Title', s) then SaveDialog.Title := s;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Dialog.Filter', s) then
    begin
    OpenDialog.Filter := s + '(*.gd)|*.gd';
    SaveDialog.Filter := OpenDialog.Filter;
    end;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Question.Caption', s) then FLngDialogCaption := s;
  if SimpleParameters_Get(@FLanguage, 'MainFrm.Question.Info', s) then FLngDialogInfo := s;


  //Экспорт задания
  if SimpleParameters_Get(@FLanguage, 'ExportTask.StartWord', s) then FLngStartWord := s;
  if SimpleParameters_Get(@FLanguage, 'ExportTask.ActionWord', s) then FLngActionWord := s;
end;


procedure TMainFrm.PrepareSettings;
begin
  //Язык
  SimpleParameters_Add(@FSettings, 'Language', 'Русский');

  //Настройки окна
  SimpleParameters_Add(@FSettings, 'Window.Left', '100');
  SimpleParameters_Add(@FSettings, 'Window.Top', '100');
  SimpleParameters_Add(@FSettings, 'Window.Width', '350');
  SimpleParameters_Add(@FSettings, 'Window.Height', '300');

  //Настройки отображения
  SimpleParameters_Add(@FSettings, 'Pattern.BGColor', '255 255 255');
  SimpleParameters_Add(@FSettings, 'Pattern.GreedColor', '200 200 200');
  SimpleParameters_Add(@FSettings, 'Pattern.GreedWidth', '1');
  SimpleParameters_Add(@FSettings, 'Pattern.StartPointColor', '255 0 0');
  SimpleParameters_Add(@FSettings, 'Pattern.StartPointRadius', '7');
  SimpleParameters_Add(@FSettings, 'Pattern.LineColor', '0 0 0');
  SimpleParameters_Add(@FSettings, 'Pattern.LineWidth', '5');
  SimpleParameters_Add(@FSettings, 'Pattern.EditorDotColor', '0 0 255');
  SimpleParameters_Add(@FSettings, 'Pattern.EditorDotRadius', '5');
  SimpleParameters_Add(@FSettings, 'Pattern.NewLineColor', '255 0 0');
  SimpleParameters_Add(@FSettings, 'Pattern.NewLineWidth', '3');
  SimpleParameters_Add(@FSettings, 'Pattern.NewLineDotColor', '255 0 0');
  SimpleParameters_Add(@FSettings, 'Pattern.NewLineDotRadius', '5');
end;


procedure TMainFrm.LoadSettings;
var
  fn: String;
begin
  fn := ExtractFilePath(Application.ExeName) + 'GD.Settings';
  SimpleParameters_UpdateFromFile(@FSettings, fn);
end;


procedure TMainFrm.SaveSettings;
var
  fn: String;
begin
  fn := ExtractFilePath(Application.ExeName) + 'GD.Settings';

  //Язык
  SimpleParameters_Set(@FSettings, 'Language', FLanguageName);

  //Настройки окна
  SimpleParameters_Set(@FSettings, 'Window.Left', Left);
  SimpleParameters_Set(@FSettings, 'Window.Top', Top);
  SimpleParameters_Set(@FSettings, 'Window.Width', Width);
  SimpleParameters_Set(@FSettings, 'Window.Height', Height);

  //Настройки отображения
  SimpleParameters_Set(@FSettings, 'Pattern.BGColor', GetColorByString(FVisCfg.PatternBGColor));
  SimpleParameters_Set(@FSettings, 'Pattern.GreedColor', GetColorByString(FVisCfg.GreedColor));
  SimpleParameters_Set(@FSettings, 'Pattern.GreedWidth', FVisCfg.GreedWidth);
  SimpleParameters_Set(@FSettings, 'Pattern.StartPointColor', GetColorByString(FVisCfg.StartPointColor));
  SimpleParameters_Set(@FSettings, 'Pattern.StartPointRadius', FVisCfg.StartPointRadius);
  SimpleParameters_Set(@FSettings, 'Pattern.LineColor', GetColorByString(FVisCfg.LineColor));
  SimpleParameters_Set(@FSettings, 'Pattern.LineWidth', FVisCfg.LineWidth);
  SimpleParameters_Set(@FSettings, 'Pattern.EditorDotColor', GetColorByString(FVisCfg.EditorDotColor));
  SimpleParameters_Set(@FSettings, 'Pattern.EditorDotRadius', FVisCfg.EditorDotRadius);
  SimpleParameters_Set(@FSettings, 'Pattern.NewLineColor', GetColorByString(FVisCfg.NewLineColor));
  SimpleParameters_Set(@FSettings, 'Pattern.NewLineWidth', FVisCfg.NewLineWidth);
  SimpleParameters_Set(@FSettings, 'Pattern.NewLineDotColor', GetColorByString(FVisCfg.NewLineDotColor));
  SimpleParameters_Set(@FSettings, 'Pattern.NewLineDotRadius', FVisCfg.NewLineDotRadius);

  if not SimpleParameters_UpdateInFile(@FSettings, fn, True, [], sa_StrDivider, True) then
    raise Exception.Create('Unable save settings to file "' + fn + '"');
end;


procedure TMainFrm.ApplySettings;
begin
  //Окно
  FLanguageName := GetStrValueDefault('Language', 'Russian');

  //Окно
  Left := GetIntValueDefault('Window.Left', 100);
  Top := GetIntValueDefault('Window.Top', 100);
  Width := GetIntValueDefault('Window.Width', 350);
  Height := GetIntValueDefault('Window.Height', 300);

  //Внешний вид
  FVisCfg.PatternBGColor := GetColValueDefault('Pattern.BGColor', clWhite);
  FVisCfg.GreedColor := GetColValueDefault('Pattern.GreedColor', RGBToColor(200, 200, 200));
  FVisCfg.GreedWidth := GetIntValueDefault('Pattern.GreedWidth', 1);
  FVisCfg.StartPointColor := GetColValueDefault('Pattern.StartPointColor', RGBToColor(255, 0, 0));
  FVisCfg.StartPointRadius := GetIntValueDefault('Pattern.StartPointRadius', 7);
  FVisCfg.LineColor := GetColValueDefault('Pattern.LineColor', RGBToColor(0, 0, 0));
  FVisCfg.LineWidth := GetIntValueDefault('Pattern.LineWidth', 5);
  FVisCfg.EditorDotColor := GetColValueDefault('Pattern.EditorDotColor', RGBToColor(0, 0, 255));
  FVisCfg.EditorDotRadius := GetIntValueDefault('Pattern.EditorDotRadius', 5);
  FVisCfg.NewLineColor := GetColValueDefault('Pattern.NewLineColor', RGBToColor(255, 0, 0));
  FVisCfg.NewLineWidth := GetIntValueDefault('Pattern.NewLineWidth', 3);
  FVisCfg.NewLineDotColor := GetColValueDefault('Pattern.NewLineDotColor', RGBToColor(255, 0, 0));
  FVisCfg.NewLineDotRadius := GetIntValueDefault('Pattern.NewLineDotRadius', 5);
end;


function TMainFrm.GetStrValueDefault(sName: String; DefValue: String): String;
begin
  if not SimpleParameters_Get(@FSettings, sName, Result) then Result := DefValue;
end;


function TMainFrm.GetIntValueDefault(sName: String; DefValue: Integer): Integer;
begin
  if not SimpleParameters_Get(@FSettings, sName, Result) then Result := DefValue;
end;


function TMainFrm.GetColValueDefault(sName: String; DefValue: TColor): TColor;
var
  s: String;
  R, G, B: Integer;
  sa: TStringArray;
begin
  Result := DefValue;
  if not SimpleParameters_Get(@FSettings, sName, s) then   Exit;

  //Разобрать строку
  R := 0;
  G := 0;
  B := 0;
  StringArray_StringToArray(@sa, Trim(s), ' ');
  if StringArray_Equal(@sa, 3) then
    begin
    if not TryStrToInt(sa[0], R) then R := 0;
    if not TryStrToInt(sa[1], G) then G := 0;
    if not TryStrToInt(sa[2], B) then B := 0;
    Result := RGBToColor(R, G, B);
    end;
  StringArray_Clear(@sa);
end;


procedure TMainFrm.SplitterChangeBounds(Sender: TObject);
begin
  CorrectBitmapSize(PaintBox.Width, PaintBox.Height, FBmp, FPattern);
  PaintBox.Repaint;
end;



procedure TMainFrm.SetEditorMode(AMode: Boolean);
begin
  FEditMode := AMode;

  mEditMode.Checked := FEditMode;
  PaintBox.Cursor := crArrow;
  PaintCanvas;
end;



end.



