unit FigureClass;

interface

uses Graphics, SysUtils;

type
  PPoint = ^TDot;
  PCircle = ^TCircle;

  TDot = class
  private { приватные поля и методы }
    X: integer; { координата X }
    Y: integer; { координата Y }
    Color: integer; { цвет }
    Size: integer; { размер }
    Angle: integer; { угол }
    Canvas: TCanvas; // на нем рисуем
    FSelected: boolean;
    procedure SetSelection(Value: boolean);
    procedure DrawBorder(AColor: integer); virtual;
  public
    procedure Draw; virtual; { показать }
    procedure Hide; virtual; { стереть }
    constructor Create(aX, aY, AColor: integer; ACanvas: TCanvas;
      aSize: integer = 1);
    { создать }
    destructor Destroy; override; { удалить }
    procedure Shift(aX, aY: integer); { подвинуть }
    procedure ChangeSize(AScale: double); virtual; abstract; { изменить размер }
    procedure Rotate(AAngle: integer); virtual; abstract; { повернуть }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    property Selected: boolean read FSelected write SetSelection;
    function Info: string; virtual;
  end;

  TCircle = class(TDot)
  private
    procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;

    procedure Hide; override;
    procedure ChangeSize(AScale: double);
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TSquare = class(TDot)
  private
    procedure DrawBorder(AColor: integer); override;
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(AScale: double);
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  PListItem = ^ListItem;

  ListItem = record
    Item: TDot;
    NextItem: PListItem;
  end;

  TList = class
  public
    FirstItem: PListItem;
    constructor Create(AFirstItem: PListItem = nil);
    destructor Destroy; override;
    procedure AddItem(AItem: TDot);
  end;

implementation

constructor TDot.Create(aX, aY, AColor: integer; ACanvas: TCanvas;
  aSize: integer = 1);
begin
  { инициализируем поля экземпляра класса }
  X := aX;
  Y := aY;
  Canvas := ACanvas;
  Canvas.Brush.Style := bsClear;
  Color := AColor;
  Size := aSize;
  Draw;
  Selected := true;
end;

destructor TDot.Destroy;
begin
  { сотрем с экрана класс }
  Selected := False;
  Hide;
  inherited;
end;

procedure TDot.Hide;
var
  TmpColor: integer;
begin
  { нарисуем класс с 0-м цветом (стираем) }
  if FSelected then
    DrawBorder($FFFFFF);
  TmpColor := Color;
  Color := $FFFFFF;
  Draw;
  Color := TmpColor;
end;

function TDot.Info: string;
begin
  result := ClassName + ' X:' + IntToStr(X) + ' Y:' + IntToStr(Y);
end;

function TDot.PointInFugure(aX, aY: integer): boolean;
begin
  result := (aX = X) and (aY = Y);
end;

procedure TDot.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    DrawBorder($00FF00)
  else
    DrawBorder($FFFFFF);
end;

procedure TDot.Shift(aX, aY: integer);
begin
  Hide; { сотрем со старыми координатами }
  X := aX;
  Y := aY;
  Draw; { нарисуем с новыми координатами }
  if FSelected then
    DrawBorder($00FF00);
end;

procedure TDot.Draw;
begin
  { рисуем класс }
  Canvas.Pixels[X, Y] := Color;
end;

procedure TDot.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
end;

{ TCircle }

procedure TCircle.ChangeSize(AScale: double);
begin
  Hide;
  Size := Round(Size * AScale);
  Draw;
end;

destructor TCircle.Destroy;
begin
  inherited;
end;

procedure TCircle.Draw;
begin

  Canvas.Pen.Color := Color;
  Canvas.Ellipse(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TCircle.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Ellipse(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Ellipse(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
end;

procedure TCircle.Hide;
begin
  inherited;
end;

function TCircle.PointInFugure(aX, aY: integer): boolean;
begin
  result := (((aX - X) * (aX - X) + (aY - Y) * (aY - Y)) <= Size * Size);
end;

{ TList }

procedure TList.AddItem(AItem: TDot);
var
  ListItem, Tmp: PListItem;
begin
  New(ListItem);
  ListItem.Item := AItem;
  ListItem.NextItem := nil;

  if FirstItem = nil then
    FirstItem := ListItem
  else
  begin
    Tmp := FirstItem;
    while Tmp.NextItem <> nil do
      Tmp := Tmp.NextItem;
    Tmp.NextItem := ListItem;
  end;
end;

constructor TList.Create(AFirstItem: PListItem);
begin
  FirstItem := AFirstItem;
end;

destructor TList.Destroy;
var
  ListItem, NextItem: PListItem;
begin
  if FirstItem <> nil then
  begin
    ListItem := FirstItem;
    repeat
      NextItem := ListItem.NextItem;
      FreeAndNil(ListItem.Item);
      Dispose(ListItem);
      ListItem := NextItem;
    until ListItem = nil;
  end;
  inherited;
end;

{ TSquare }

procedure TSquare.ChangeSize(AScale: double);
begin
  Hide;
  Size := Round(Size * AScale);
  Draw;
end;

destructor TSquare.Destroy;
begin

  inherited;
end;

procedure TSquare.Draw;
begin
  // inherited;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(X - Size, Y - Size, X + Size, Y + Size);
end;

procedure TSquare.DrawBorder(AColor: integer);
begin
  Canvas.Pen.Color := AColor;
  Canvas.Rectangle(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Rectangle(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
end;

procedure TSquare.Hide;
begin
  inherited;

end;

function TSquare.PointInFugure(aX, aY: integer): boolean;
begin
  result := not((aX > X + Size) or (aY > Y + Size) or (aX < X - Size) or
      (aY < Y - Size))
end;

end.
