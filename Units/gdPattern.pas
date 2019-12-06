unit gdPattern;

{$mode objfpc}{$H+}{$M+}

interface


uses
  SysUtils, Types;


type
  //Массив точек
  TgdPointArray = array of TPoint;
  PgdPointArray = ^TgdPointArray;


  //Рисунок
  TgdPattern = class
  private
    FPoints: TgdPointArray;   //Массив точек
    FWidth: Word;             //Ширина в клетках
    FHeight: Word;            //Высота в клетках
    FStartPoint: TPoint;      //Начальная точка

    FSelectPointIndex: Integer;    //Номер выделенной точки

    FName: ShortString;
    FAutor: ShortString;
    FBirthday: ShortString;

    function  GetPointCount: Integer;
    function  StringToPoint(Str: String): TPoint;
    function  PointFromString(Str: String): TPoint;

    function  GetRawPoint(Index: Integer): TPoint;
    procedure SetRawPoint(Index: Integer; Point: TPoint);
    function  GetPoint(Index: Integer): TPoint;

    procedure SetWidth(aWidth: Word);
    procedure SetHeight(aHeight: Word);
    procedure SetSelectPointIndex(Index: Integer);
  public
    constructor Create(Name: ShortString; Width, Height: Integer; StartPoint: TPoint);
    destructor  Destroy; override;

    procedure ClearPoints;
    procedure ResetStartPoint;
    procedure AddPoint(Point: TPoint);
    procedure AddPoint(StepX, StepY: Integer);
    function  PointToString(Point: TPoint; Up, Down, Left, Right: String): String;


    function  GetLastPoint: TPoint;
    function  GetPointByIdx(Index: Integer): TPoint;
    procedure DeleteLastPoint;
    function  GetPointIndexByPos(aPoint: TPoint): Integer;

    procedure FitSize;

    procedure FromString(Str: String);
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    Function  GetMaxPoint: TPoint;

    property Name: ShortString read FName write FName;
    property Autor: ShortString read FAutor write FAutor;
    property Birthday: ShortString read FBirthday write FBirthday;
    property Width: Word read FWidth write SetWidth;
    property Height: Word read FHeight write SetHeight;


    property PointCount: Integer read GetPointCount;
    property SelectPointIndex: Integer read FSelectPointIndex write SetSelectPointIndex;
    property StartPoint: TPoint read FStartPoint write FStartPoint;
    property Point[Index: Integer]: TPoint read GetPoint;
    property RawPoint[Index: Integer]: TPoint read GetRawPoint write SetRawPoint;
  end;



implementation

uses
  StringArray, SimpleParameters, SimpleContainers,
  gdFunctions;




function TgdPattern.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;


//1 Right, 1 Down
//Переделать говнокод
function TgdPattern.StringToPoint(Str: String): TPoint;
var
  sa, sb: TStringArray;
  Action1, Action2, Step, Direction: String;
  i: Integer;
begin
  Result := Types.Point(0, 0);

  //Подготовить 2 части
  StringArray_StringToArray(@sa, Str, ',');  //Разбить по ,
  Action1 := '';
  Action2 := '';
  if StringArray_Equal(@sa, 1) then Action1 := Trim(sa[0]);
  if StringArray_Equal(@sa, 2) then Action2 := Trim(sa[1]);
  StringArray_Clear(@sa);


  //Разобрать 1 часть
  StringArray_StringToArray(@sb, Action1, #32);
  if StringArray_Equal(@sb, 2) then
    begin
    Step := Trim(sb[0]);                  //Шаг
    Direction := LowerCase(Trim(sb[1]));  //Знак

    if not TryStrToInt(Step, i) then
      raise Exception.Create('Cant determine step "' + sb[0] + '"');

    case Direction of
      'right' : Result.X := i;
      'left'  : Result.X := -i;
      'down'  : Result.Y := i;
      'up'    : Result.Y := -i;
      else raise Exception.Create('Cant determine direction "' + sb[1] + '"');
    end;
    end;
  StringArray_Clear(@sb);


  //Разобрать 2 часть
  StringArray_StringToArray(@sb, Action2, #32);
  if StringArray_Equal(@sb, 2) then
    begin
    Step := Trim(sb[0]);                  //Шаг
    Direction := LowerCase(Trim(sb[1]));  //Знак

    if not TryStrToInt(Step, i) then
      raise Exception.Create('Cant determine step "' + sb[0] + '"');

    case Direction of
      'right' : Result.X := i;
      'left'  : Result.X := -i;
      'down'  : Result.Y := i;
      'up'    : Result.Y := -i;
      else raise Exception.Create('Cant determine direction "' + sb[1] + '"');
    end;
    end;
  StringArray_Clear(@sb);
end;


function TgdPattern.PointFromString(Str: String): TPoint;
var
  sa: TStringArray;
begin
  Result := Types.Point(0 ,0);

  StringArray_StringToArray(@sa, Str, ','); //Распилить по ,
  if StringArray_Equal(@sa, 2) then
    begin
    if not TryStrToInt(sa[0], Result.X) then Result.X := 0;
    if not TryStrToInt(sa[1], Result.Y) then Result.Y := 0;
    end;
  StringArray_Clear(@sa);
end;


function TgdPattern.GetRawPoint(Index: Integer): TPoint;
var
  c: Integer;
begin
  c := GetPointCount - 1;
  if (Index < 0) or (Index > c) then
    raise Exception.Create('Index out of bounds "' + IntToStr(Index) + '"');

  Result := FPoints[Index];
end;

procedure TgdPattern.SetRawPoint(Index: Integer; Point: TPoint);
var
  c: Integer;
begin
  c := GetPointCount - 1;
  if (Index < 0) or (Index > c) then
    raise Exception.Create('Index out of bounds "' + IntToStr(Index) + '"');

  FPoints[Index] := Point;
end;


function TgdPattern.GetPoint(Index: Integer): TPoint;
var
  c, i: Integer;
begin
  c := GetPointCount - 1;
  if (Index < 0) or (Index > c) then
    raise Exception.Create('Index out of bounds "' + IntToStr(Index) + '"');


  Result := FStartPoint;
  for i := 0 to c do
    begin
    Result.X := Result.X + FPoints[i].X;
    Result.Y := Result.Y + FPoints[i].Y;
    end;
end;


procedure TgdPattern.SetWidth(aWidth: Word);
begin
  if aWidth < 1 then aWidth := 1;
  FWidth := aWidth;
end;


procedure TgdPattern.SetHeight(aHeight: Word);
begin
  if aHeight < 1 then aHeight := 1;
  FHeight := aHeight;
end;


procedure TgdPattern.SetSelectPointIndex(Index: Integer);
var
  c: Integer;
begin
  c := GetPointCount - 1;
  if (Index < 0) or (Index > c) then Index := -1;

  FSelectPointIndex := Index;
end;


constructor TgdPattern.Create(Name: ShortString; Width, Height: Integer; StartPoint: TPoint);
begin
  FName := Name;
  SetWidth(Width);
  SetHeight(Height);
  FStartPoint := StartPoint;
  FSelectPointIndex := -1;

  FAutor := '';
  FBirthday := FormatDateTime('dd.mm.yyyy', Now);
end;


destructor TgdPattern.Destroy;
begin
  ClearPoints;
end;


procedure TgdPattern.ClearPoints;
begin
  SetLength(FPoints, 0);
end;


procedure TgdPattern.ResetStartPoint;
begin
  FStartPoint := Types.Point(0, 0);
end;


procedure TgdPattern.AddPoint(Point: TPoint);
var
  c: Integer;
begin
  c := GetPointCount;
  SetLength(FPoints, c + 1);
  FPoints[c] := Point;
end;


procedure TgdPattern.AddPoint(StepX, StepY: Integer);
begin
  if (StepX = 0) and (StepY = 0) then
    raise Exception.Create('Point is empty');

  AddPoint(Types.Point(StepX, StepY));
end;


function TgdPattern.PointToString(Point: TPoint; Up, Down, Left, Right: String): String;
var
  s: String;
begin
  Result := '';
  //X
  if Point.X <> 0 then
    begin
    if Point.X > 0 then s := Right else s := Left;
    Result := Result + IntToStr(Abs(Point.X)) + s;
    end;

  //Y
  if Point.Y <> 0 then
    begin
    if Point.Y > 0 then s := Down else s := Up;
    Result := Result + IntToStr(Abs(Point.Y)) + s;
    end;


  {//X
  if Point.X <> 0 then
    if Point.X > 0 then sx := IntToStr(Point.X) + ' Right' else sx := IntToStr(Abs(Point.X)) + ' Left';

  //Y
  if Point.Y <> 0 then
    if Point.Y > 0 then sy := IntToStr(Point.Y) + ' Down' else sy := IntToStr(Abs(Point.Y)) + ' Up';

  Result := sx;
  if (sx <> '') and (sy <> '') then Result := Result + ', ';
  Result := Result + sy;}
end;


function TgdPattern.GetLastPoint: TPoint;
var
  c, i: Integer;
begin
  Result := FStartPoint;

  c := GetPointCount - 1;
  if c < 0 then Exit;

  for i := 0 to c do
    begin
    Result.X := Result.X + FPoints[i].X;
    Result.Y := Result.Y + FPoints[i].Y;
    end;
end;


function TgdPattern.GetPointByIdx(Index: Integer): TPoint;
var
  c, i: Integer;
begin
  Result := FStartPoint;

  c := GetPointCount - 1;
  if (Index < 0) or (Index > c) then Exit;

  for i := 0 to Index do
    begin
    Result.X := Result.X + FPoints[i].X;
    Result.Y := Result.Y + FPoints[i].Y;
    end;
end;


procedure TgdPattern.DeleteLastPoint;
var
  c: Integer;
begin
  c := GetPointCount - 1;
  if c < 0 then Exit;

  SetLength(FPoints, c);
end;


function TgdPattern.GetPointIndexByPos(aPoint: TPoint): Integer;
var
  i, c, PtX, PtY: Integer;
begin
  Result := -1;

  PtX := FStartPoint.X;
  PtY := FStartPoint.Y;

  c := GetPointCount - 1;
  for i := 0 to c do
    begin
    PtX := PtX + FPoints[i].X;
    PtY := PtY + FPoints[i].Y;

    if (PtX = aPoint.X) and (PtY = aPoint.Y) then
      begin
      Result := i;
      Break;
      end;
    end;
end;


procedure TgdPattern.FitSize;
var
  c, i, W, H, PtX, PtY: Integer;
  MinX, MinY, MaxX, MaxY: Integer;
begin
  MinX := FWidth;
  MinY := FHeight;
  MaxX := 0;
  MaxY := 0;

  //Определить границы узора
  PtX := FStartPoint.X;
  PtY := FStartPoint.Y;
  if PtX < MinX then MinX := PtX;
  if PtX > MaxX then MaxX := PtX;
  if PtY < MinY then MinY := PtY;
  if PtY > MaxY then MaxY := PtY;

  c := GetPointCount - 1;
  for i := 0 to c do
    begin
    PtX := PtX + FPoints[i].X;
    PtY := PtY + FPoints[i].Y;
    if PtX < MinX then MinX := PtX;
    if PtX > MaxX then MaxX := PtX;
    if PtY < MinY then MinY := PtY;
    if PtY > MaxY then MaxY := PtY;
    end;

  //Определить размеры узора
  W := MaxX - MinX;
  H := MaxY - MinY;
  if W < 1 then W := 1;
  if H < 1 then H := 1;
  FWidth := W;
  FHeight := H;

  //Поправить стартовую точку если она не в 0,0
  FStartPoint.X := FStartPoint.X - MinX;
  FStartPoint.Y := FStartPoint.Y - MinY;
end;


procedure TgdPattern.FromString(Str: String);
var
  sp, Info: TSimpleParameters;
  Action: TStringArray;
  sInfo, sAction: String;

  iName, iAutor, iBirthday, iPoint: String;
  aPoint, pt: TPoint;
  iWidth, iHeight, c, i, k: Integer;
  PtArr: TgdPointArray;

begin
  //Разбить строку на параметры
  SimpleContainers_FromString(@sp, Str);

  //Чтение секций
  sInfo := '';
  if not SimpleParameters_Get(@sp, 'Info', sInfo) then
    raise Exception.Create('Cant read "Info" section');
  SimpleParameters_FromString(@Info, Trim(sInfo));

  sAction := '';
  if not SimpleParameters_Get(@sp, 'Action', sAction) then
    raise Exception.Create('Cant read "Action" section');
  StringArray_StringToArray(@Action, Trim(sAction));
  SimpleParameters_Clear(@sp);


  //Разбор Info
  //Имя
  iName := '';
  SimpleParameters_Get(@Info, 'Name', iName);

  //Автор
  iAutor := '';
  SimpleParameters_Get(@Info, 'Autor', iAutor);

  //День варенья
  iBirthday := '';
  SimpleParameters_Get(@Info, 'Birthday', iBirthday);

  //Стартовая точка
  iPoint := '';
  SimpleParameters_Get(@Info, 'StartPoint', iPoint);
  aPoint := PointFromString(iPoint);

  //Ширина в клетках
  if not SimpleParameters_Get(@Info, 'Width', iWidth) then
    raise Exception.Create('Cant read "Width" from "Info" section');

  //Высота в клетках
  if not SimpleParameters_Get(@Info, 'Height', iHeight) then
    raise Exception.Create('Cant read "Height" from "Info" section');
  SimpleParameters_Clear(@Info);


  //Разбор Action
  c := StringArray_GetCount(@Action) - 1;
  for i := 0 to c do
    begin

    //Попробывать преобразовать строку в действие
    try
      pt := StringToPoint(Action[i]);
    except
      on E: Exception do
        raise Exception.Create('Cant read data from "Action" section');
    end;

    //Добавить массив
    k := Length(PtArr);
    SetLength(PtArr, k + 1);
    PtArr[k] := pt;
    end;
  StringArray_Clear(@Action);


  //Присвоить параметры если всё успешно
  FName := iName;
  FAutor := iAutor;
  FBirthday := iBirthday;
  FStartPoint := aPoint;
  FWidth := iWidth;
  FHeight := iHeight;

  //Заменить точки
  ClearPoints;
  FPoints := PtArr;
end;


procedure TgdPattern.SaveToFile(FileName: String);
var
  Info, Fl: TSimpleParameters;
  Action: TStringArray;
  c, i: Integer;
begin
  //Заплатка автозамена имени при сохранении
  FName := ChangeFileExt(ExtractFileName(FileName), '');


  //Info
  SimpleParameters_Add(@Info, 'Name', FName);
  SimpleParameters_Add(@Info, 'Autor', FAutor);
  SimpleParameters_Add(@Info, 'Birthday', FBirthday);
  SimpleParameters_Add(@Info, 'Width', IntToStr(FWidth));
  SimpleParameters_Add(@Info, 'Height', IntToStr(FHeight));
  SimpleParameters_Add(@Info, 'StartPoint', IntToStr(FStartPoint.X) + ', ' + IntToStr(FStartPoint.Y));

  //Action
  StringArray_Add(@Action, '');
  c := GetPointCount - 1;
  for i := 0 to c do
    StringArray_Add(@Action, PointToString(FPoints[i], 'Up', 'Down', 'Left', 'Right'));
  StringArray_Add(@Action, '');

  //Save
  SimpleParameters_Add(@Fl, 'Info', #13#10 + SimpleParameters_ToString(@Info, True) + #13#10);
  SimpleParameters_Add(@Fl, 'Action', StringArray_ArrayToString(@Action));

  try
    if not SimpleContainers_SaveToFile(@Fl, FileName) then
      raise Exception.Create('Cant save image to file! "' + FileName + '"');
  finally
    StringArray_Clear(@Action);
    SimpleParameters_Clear(@Info);
    SimpleParameters_Clear(@Fl);
  end;
end;


procedure TgdPattern.LoadFromFile(FileName: String);
var
  Str: String;

begin
  if not LoadStringFromFile(FileName, Str) then
    raise Exception.Create('Cant load image from file! "' + FileName + '"');

  FromString(Str);
end;


function TgdPattern.GetMaxPoint: TPoint;
var
  i, c, l: Integer;
begin
  Result := Types.Point(0, 0);

  c := GetPointCount - 1;
  for i := 0 to c do
    begin
    l := Length(IntToStr(Abs(FPoints[i].X))); //Сколько символов в числе
    if l > Result.X then Result.X := l;
    l := Length(IntToStr(Abs(FPoints[i].Y))); //Сколько символов в числе
    if l > Result.Y then Result.Y := l;
    end;
end;


end.

