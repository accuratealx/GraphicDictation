unit gdFunctions;

{$mode objfpc}{$H+}

interface

uses
  Types, Graphics, SysUtils,
  gdPattern;


const
  UPArrow = '▲';
  DOWNArrow = '▼';
  LEFTArrow = '◄';
  RIGHTArrow = '►';


type
  TgdFloatPoint = record
    X, Y: Single;
  end;


  TgdRGB = record
    R, G, B: Byte;
  end;


  //Настройки вывода задания
  TExportTaskConfig = record
    //Страница
    PaperWidth: Integer;
    PaperHeight: Integer;
    DPI: Integer;
    TopIndent,
    LeftIndent,
    RightIndent,
    BottomIndent: Integer;
    BGColor: TColor;

    //Высота узора
    PatternHeight: Integer;

    //Заголовок
    CaptionFontName: String;
    CaptionFontColor: TColor;
    CaptionFontSize: Integer;
    CaptionFontStyle: TFontStyles;
    CaptionTopIndent,
    CaptionBottomIndent: Integer;

    //Стартовая точка
    StartPointFontName: String;
    StartPointFontColor: TColor;
    StartPointFontSize: Integer;
    StartPointFontStyle: TFontStyles;
    StartPointTopIndent,
    StartPointBottomIndent: Integer;

    //Решётка
    GreedColor: TColor;
    GreedLineWidth: Integer;
    GreedIndent: Integer;

    //Действия
    ActionFontName: String;
    ActionFontColor: TColor;
    ActionFontSize: Integer;
    ActionFontStyle: TFontStyles;

    //Видимые части
    VisiblePattern: Boolean;
    VisibleTask: Boolean;
  end;


  //Настройки отображения
  TgdVisualSettings = record
    PatternBGColor: TColor;
    GreedColor: TColor;
    GreedWidth: Integer;
    StartPointColor: TColor;
    StartPointRadius: Integer;
    LineColor: TColor;
    LineWidth: Integer;
    EditorDotColor: TColor;
    EditorDotRadius: Integer;
    NewLineColor: TColor;
    NewLineWidth: Integer;
    NewLineDotColor: TColor;
    NewLineDotRadius: Integer;
  end;
  PgdVisualSettings = ^TgdVisualSettings;



function  IsPointEqual(P1, P2: TPoint): Boolean;
function  GetColorByString(Color: TColor): String;
function  GetDefaultExportTaskSettings: TExportTaskConfig;
procedure PrepareExportBmp(Bmp: TBitmap; ExportSettings: TExportTaskConfig; VisSettings: TgdVisualSettings; Pattern: TgdPattern; StartWord, ActionWord: String);
function  LoadStringFromFile(FileName: String; var Str: String): Boolean;
procedure CorrectBitmapSize(ViewWidth: Integer; ViewHeight: Integer; Bmp: TBitmap; Pattern: TgdPattern);
function  GetCellSize(Bmp: TBitmap; Pattern: TgdPattern): TgdFloatPoint;
procedure DrawGreed(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; PenWidth: Integer = 1);
procedure DrawPattern(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; PenWidth: Integer = 5);
procedure DrawDots(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; Radius: Integer = 5);
procedure DrawStartPoint(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; Radius: Integer = 5);
procedure DrawSelectPoint(Bmp: TBitmap; Pattern: TgdPattern; Point: TPoint; Color: TColor; Radius: Integer = 5; LineWidth: Integer = 3);
procedure DrawNextLine(Bmp: TBitmap; Pattern: TgdPattern; LastPoint: TPoint; LineColor, DotColor: TColor; LineWidth: Integer = 5; Radius: Integer = 5);


implementation


//Пропорционально вписывает прямоугольник с размерами W и H в прямоугольник
//с размерами W0 и H0, результат кладётся в Wr и Hr
procedure InscribeRectangle(Var Wr, Hr: Integer; W0, H0: Integer; W, H: Integer);
begin
  if (W0 / H0) > (W / H) then
    begin
    Hr := H0;
    Wr := Round(H0 * W / H);
    end else
    begin
    Wr := W0;
    Hr := Round(W0 * H / W);
    end;
end;


function IsPointEqual(P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;


function GetColorByString(Color: TColor): String;
var
  R, G, B: Byte;
begin
  RedGreenBlue(Color, R, G, B);
  Result := IntToStr(R) + #32 + IntToStr(G) + #32 + IntToStr(B);
end;


function GetDefaultExportTaskSettings: TExportTaskConfig;
begin
  with Result do
    begin
    PaperWidth := 210;
    PaperHeight := 297;
    DPI := 300;
    TopIndent := 5;
    LeftIndent := 10;
    RightIndent := 10;
    BottomIndent := 5;
    BGColor := clWhite;

    //Узор
    PatternHeight := 150;

    //Заголовок
    CaptionFontName := 'Times new roman';
    CaptionFontColor := clBlack;
    CaptionFontSize := 60;
    CaptionFontStyle := [fsBold];
    CaptionTopIndent := 0;
    CaptionBottomIndent := 0;

    //Стартовая точка
    StartPointFontName := 'Times new roman';
    StartPointFontColor := clBlack;
    StartPointFontSize := 44;
    StartPointFontStyle := [fsBold];
    StartPointTopIndent := 3;
    StartPointBottomIndent := 2;

    //Решётка действий
    GreedColor := clBlack;
    GreedLineWidth := 1;
    GreedIndent := 1;

    //Действия
    ActionFontName := 'Times new roman';
    ActionFontColor := clBlack;
    ActionFontSize := 36;
    ActionFontStyle := [];

    //Видимые части
    VisiblePattern := True;
    VisibleTask := True;
    end;
end;


procedure PrepareExportBmp(Bmp: TBitmap; ExportSettings: TExportTaskConfig; VisSettings: TgdVisualSettings; Pattern: TgdPattern; StartWord, ActionWord: String);
var
  Delta: Single;
  rPageIndent: TRect;
  X, Y: Integer;
  bmPattern: TBitmap;
  pgWidth, pgHeight: Integer;
  cptTopIndent, cptBottomIndent, cptWidth, cptHeight, cptX, cptY: Integer;
  ptrnWidth, ptrnHeight, ptrnX, ptrnY: Integer;
  spTopIndent, spBottomIndent, spHeight, spWidth: Integer;
  grdIndent, grdWidth: Integer;
  cellWidth, cellHeight, cellPerWidth: Integer;
  Str: String;
  i, c, j: Integer;
  pt: TPoint;
begin
  //Подготовить разметку
  Delta := ExportSettings.DPI / 25.4; //Пикселей по DPI

  //Размеры листа
  pgWidth := Round(ExportSettings.PaperWidth * Delta);
  pgHeight := Round(ExportSettings.PaperHeight * Delta);

  //Подготовить лист
  Bmp.Width := pgWidth;
  Bmp.Height := pgHeight;
  Bmp.Canvas.Brush.Color := ExportSettings.BGColor;
  Bmp.Canvas.Brush.Style := bsSolid;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);

  //Отступы на листе
  rPageIndent.Top := Round(ExportSettings.TopIndent * Delta);
  rPageIndent.Left := Round(ExportSettings.LeftIndent * Delta);
  rPageIndent.Right := Round(ExportSettings.RightIndent * Delta);
  rPageIndent.Bottom := Round(ExportSettings.BottomIndent * Delta);

  //Заголовок
  with Bmp.Canvas, ExportSettings do
    begin
    Font.Name := CaptionFontName;
    Font.Size := CaptionFontSize;
    Font.Color := CaptionFontColor;
    Font.Style := CaptionFontStyle;
    cptTopIndent := Round(Delta * ExportSettings.CaptionTopIndent);
    cptBottomIndent := Round(Delta * ExportSettings.CaptionBottomIndent);
    cptWidth := TextWidth(Pattern.Name);
    cptHeight := TextHeight(Pattern.Name);
    cptX := rPageIndent.Left + (pgWidth - rPageIndent.Left - rPageIndent.Right) div 2 - cptWidth div 2;
    cptY := rPageIndent.Top + (cptHeight + cptTopIndent + cptBottomIndent) div 2 - cptHeight div 2;
    TextOut(cptX, cptY, Pattern.Name);
    end;

  //Вывод рамки
  //with Bmp.Canvas do
  //  begin
  //  Pen.Width := 1;
  //  Pen.Color := clBlack;
  //  Pen.Style := psSolid;
  //  Brush.Style := bsClear;
  //  Bmp.Canvas.Rectangle(rPageIndent.Left, rPageIndent.Top, Bmp.Width - rPageIndent.Right, Bmp.Height - rPageIndent.Bottom);
  //  end;

  //Вывод узора
  bmPattern := TBitmap.Create;
  ptrnWidth := Round(Bmp.Width - rPageIndent.Left - rPageIndent.Right);
  ptrnHeight := Round(Delta * ExportSettings.PatternHeight);
  CorrectBitmapSize(ptrnWidth, ptrnHeight, bmPattern, Pattern);
  bmPattern.Canvas.Brush.Color := VisSettings.PatternBGColor;
  bmPattern.Canvas.Brush.Style := bsSolid;
  bmPattern.Canvas.FillRect(0, 0, bmPattern.Width, bmPattern.Height);
  DrawGreed(bmPattern, Pattern, VisSettings.GreedColor, VisSettings.GreedWidth);
  if ExportSettings.VisiblePattern then
    DrawPattern(bmPattern, Pattern, VisSettings.LineColor, VisSettings.LineWidth);                //Линии узора
  DrawStartPoint(bmPattern, Pattern, VisSettings.StartPointColor, VisSettings.StartPointRadius);
  ptrnX := rPageIndent.Left + ptrnWidth div 2 - bmPattern.Width div 2;
  ptrnY := rPageIndent.Top + cptHeight + cptTopIndent + cptBottomIndent + (ptrnHeight div 2) - bmPattern.Height div 2;
  Bmp.Canvas.Draw(ptrnX, ptrnY, bmPattern);
  bmPattern.Free;

  //Стартовая точка + количество действий
  Str := StartWord + ' ' + IntToStr(Pattern.StartPoint.X) + ', ' + IntToStr(Pattern.StartPoint.Y);
  Str := Str + '     ' + ActionWord + ' ' + IntToStr(Pattern.PointCount);
  with Bmp.Canvas, ExportSettings do
    begin
    Font.Name := StartPointFontName;
    Font.Size := StartPointFontSize;
    Font.Color := StartPointFontColor;
    Font.Style := StartPointFontStyle;
    spTopIndent := Round(Delta * ExportSettings.StartPointTopIndent);
    spBottomIndent := Round(Delta * ExportSettings.StartPointBottomIndent);
    spHeight := TextHeight(Str);
    spWidth := TextWidth(Str);
    X := rPageIndent.Left + (Bmp.Width - rPageIndent.Left - rPageIndent.Right) div 2 - spWidth div 2;
    Y := rPageIndent.Top + cptTopIndent + cptBottomIndent + cptHeight + ptrnHeight + (spHeight + spTopIndent + spBottomIndent) div 2 - spHeight div 2;
    TextOut(X, Y, Str);
    end;

  //Вывод действий
  with Bmp.Canvas, ExportSettings do
    begin
    Pen.Color := GreedColor;
    Pen.Width := GreedLineWidth;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Font.Name := ActionFontName;
    Font.Size := ActionFontSize;
    Font.Style := ActionFontStyle;
    Font.Color := ActionFontColor;
    pt := Pattern.GetMaxPoint;
    Str := Pattern.PointToString(pt, UPArrow, DOWNArrow, LEFTArrow, RIGHTArrow);
    grdIndent := Round(GreedIndent * Delta);
    cellWidth := TextWidth(Pattern.GetMaxLengthPointString) + grdIndent *  2;
    cellHeight := TextHeight(Str) + grdIndent * 2;
    cellPerWidth := ptrnWidth div cellWidth;
    grdWidth := ptrnWidth - cellPerWidth * cellWidth;

    X := rPageIndent.Left + grdWidth div 2;
    Y := rPageIndent.Top + cptTopIndent + cptHeight + cptBottomIndent + ptrnHeight + spTopIndent + spHeight + spBottomIndent;

    c := Pattern.PointCount - 1;
    j := 0;
    for i := 0 to c do
      begin
      Rectangle(X, Y, X + cellWidth, Y + cellHeight);
      if ExportSettings.VisibleTask then
        TextOut(X + grdIndent, Y + grdIndent, Pattern.PointToString(Pattern.RawPoint[i], UPArrow, DOWNArrow, LEFTArrow, RIGHTArrow)); //Текст
      Inc(X, cellWidth);
      Inc(j);
      if j >= cellPerWidth then
        begin
        j := 0;
        Inc(Y, cellHeight);
        X := rPageIndent.Left + grdWidth div 2;
        end;
      end;
    end;
end;


function LoadStringFromFile(FileName: String; var Str: String): Boolean;
var
  Size: Integer;
  F: file of Char;
begin
  try
    AssignFile(F, FileName);
    Reset(F);
    Size := FileSize(F);
    SetLength(Str, Size);
    if Size > 0 then BlockRead(F, Str[1], Size);
    CloseFile(F);
  except
    Result := False;
    Exit;
  end;
  Result := True;
end;


procedure CorrectBitmapSize(ViewWidth, ViewHeight: Integer; Bmp: TBitmap; Pattern: TgdPattern);
var
  W, H: Integer;
begin
  InscribeRectangle(W, H, ViewWidth, ViewHeight, Pattern.Width, Pattern.Height);

  Bmp.Width := W;
  Bmp.Height := H;
end;


function GetCellSize(Bmp: TBitmap; Pattern: TgdPattern): TgdFloatPoint;
begin
  Result.X := Bmp.Width / (Pattern.Width + 2);
  Result.Y := Bmp.Height / (Pattern.Height + 2);
end;


procedure DrawGreed(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; PenWidth: Integer = 1);
var
  i: Integer;
  X1, X2, Y1, Y2: Integer;
  CellSize: TgdFloatPoint;
begin
  //Размеры клеток
  CellSize := GetCellSize(Bmp, Pattern);

  //Подготовить графику
  with Bmp.Canvas do
    begin
    Pen.Width := PenWidth;
    Pen.Color := Color;
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    end;

  //Вертикальные полосы
  Y1 := Round(CellSize.Y);
  Y2 := Round(Bmp.Height - CellSize.Y);
  for i := 0 to Pattern.Width do
    begin
    X1 := Round((i + 1) * CellSize.X);
    Bmp.Canvas.Line(X1, Y1, X1, Y2);
    end;

  //Горизонтальные полосы
  X1 := Round(CellSize.X);
  X2 := Round(Bmp.Width - CellSize.X);
  for i := 0 to Pattern.Height do
    begin
    Y1 := Round((i + 1) * CellSize.Y);
    Bmp.Canvas.Line(X1, Y1, X2, Y1);
    end;
end;


procedure DrawPattern(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; PenWidth: Integer = 5);
var
  i, c: Integer;
  X, Y: Single;
  CellSize, PrevPoint: TgdFloatPoint;
begin
  //Подготовить графику
  with Bmp.Canvas do
    begin
    Pen.Width := PenWidth;
    Pen.Style := psSolid;
    Pen.Color := Color;
    end;

  //Стартовая точка
  CellSize := GetCellSize(Bmp, Pattern);
  X := CellSize.X + Pattern.StartPoint.X * CellSize.X;
  Y := CellSize.Y + Pattern.StartPoint.Y * CellSize.Y;
  PrevPoint.X := X;
  PrevPoint.Y := Y;

  //Вывод точек
  Bmp.Canvas.MoveTo(Round(X), Round(Y));
  c := Pattern.PointCount - 1;
  for i := 0 to c do
    begin
    X := PrevPoint.X + Pattern.RawPoint[i].X * CellSize.X;
    Y := PrevPoint.Y + Pattern.RawPoint[i].Y * CellSize.Y;
    Bmp.Canvas.LineTo(Round(X), Round(Y));
    PrevPoint.X := X;
    PrevPoint.Y := Y;
    end;
end;


procedure DrawDots(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; Radius: Integer = 5);
var
  i, c: Integer;
  X, Y: Single;
  CellSize, PrevPoint: TgdFloatPoint;
begin
  //Подготовить графику
  with Bmp.Canvas do
    begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := Color;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    end;

  //Стартовая точка
  CellSize := GetCellSize(Bmp, Pattern);
  X := CellSize.X + Pattern.StartPoint.X * CellSize.X;
  Y := CellSize.Y + Pattern.StartPoint.Y * CellSize.Y;
  PrevPoint.X := X;
  PrevPoint.Y := Y;

  //Вывод точек
  Bmp.Canvas.MoveTo(Round(X), Round(Y));
  c := Pattern.PointCount - 1;
  for i := 0 to c do
    begin
    X := PrevPoint.X + Pattern.RawPoint[i].X * CellSize.X;
    Y := PrevPoint.Y + Pattern.RawPoint[i].Y * CellSize.Y;
    Bmp.Canvas.EllipseC(Round(X), Round(Y), Radius, Radius);
    PrevPoint.X := X;
    PrevPoint.Y := Y;
    end;
end;


procedure DrawStartPoint(Bmp: TBitmap; Pattern: TgdPattern; Color: TColor; Radius: Integer = 5);
var
  X, Y: Integer;
  CellSize: TgdFloatPoint;
begin
  //Подготовить графику
  with Bmp.Canvas do
    begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := Color;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    end;

  //Коодинаты точки
  CellSize := GetCellSize(Bmp, Pattern);
  X := Round(CellSize.X + Pattern.StartPoint.X * CellSize.X);
  Y := Round(CellSize.Y + Pattern.StartPoint.Y * CellSize.Y);

  //Вывод стартовой точки
  Bmp.Canvas.EllipseC(X, Y, Radius, Radius);
end;


procedure DrawSelectPoint(Bmp: TBitmap; Pattern: TgdPattern; Point: TPoint; Color: TColor; Radius: Integer = 5; LineWidth: Integer = 3);
var
  X, Y: Integer;
  CellSize: TgdFloatPoint;
begin
  //Подготовить графику
  with Bmp.Canvas do
    begin
    Pen.Style := psSolid;
    Pen.Width := LineWidth;
    Pen.Color := Color;
    Brush.Style := bsClear;
    end;

  //Коодинаты точки
  CellSize := GetCellSize(Bmp, Pattern);
  X := Round(CellSize.X + Point.X * CellSize.X);
  Y := Round(CellSize.Y + Point.Y * CellSize.Y);

  //Вывод точки
  Bmp.Canvas.EllipseC(X, Y, Radius, Radius);
end;


procedure DrawNextLine(Bmp: TBitmap; Pattern: TgdPattern; LastPoint: TPoint; LineColor, DotColor: TColor; LineWidth: Integer = 5; Radius: Integer = 5);
var
  CellSize: TgdFloatPoint;
  ptOne, ptTwo: TPoint;
begin
  CellSize := GetCellSize(Bmp, Pattern);
  ptOne := Pattern.GetLastPoint;

  ptTwo := LastPoint;
  if (ptTwo.X = -1) and (ptTwo.Y = -1) then ptTwo := ptOne;

  ptOne.X := Round(CellSize.X + ptOne.X * CellSize.X);
  ptOne.Y := Round(CellSize.Y + ptOne.Y * CellSize.Y);
  ptTwo.X := Round(CellSize.X + ptTwo.X * CellSize.X);
  ptTwo.Y := Round(CellSize.Y + ptTwo.Y * CellSize.Y);

  with Bmp.Canvas do
    begin
    //Линия
    Pen.Color := LineColor;
    Pen.Style := psSolid;
    Pen.Width := LineWidth;
    MoveTo(ptOne);
    LineTo(ptTwo);

    //Точка
    Pen.Width := 1;
    Pen.Color := DotColor;
    Brush.Style := bsSolid;
    Brush.Color := DotColor;
    EllipseC(ptTwo.X, ptTwo.Y, Radius , Radius);
    end;
end;


end.

