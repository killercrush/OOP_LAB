unit FigureClass;

interface

uses Graphics, SysUtils;

type
  PPoint = ^TDot;
  PCircle = ^TCircle;

  TDot = class
  private { ��������� ���� � ������ }
    X: integer; { ���������� X }
    Y: integer; { ���������� Y }
    Color: integer; { ���� }
    Size: integer; { ������ }
    Angle: integer; { ���� }
    Canvas: TCanvas; // �� ��� ������
    FSelected: boolean;
    procedure SetSelection(Value: boolean); virtual;
  public
    procedure Draw; virtual; { �������� }
    procedure Hide; virtual; { ������� }
    constructor Create(aX, aY, aColor: integer; ACanvas: TCanvas;
      aSize: integer = 1);
    { ������� }
    destructor Destroy; override; { ������� }
    procedure Shift(aX, aY: integer); { ��������� }
    procedure ChangeSize(AScale: double); virtual; abstract; { �������� ������ }
    procedure Rotate(AAngle: integer); virtual; abstract; { ��������� }
    function PointInFugure(aX, aY: integer): boolean; virtual;
    property Selected: boolean read FSelected write SetSelection;
  end;

  TCircle = class(TDot)
  private
    procedure SetSelection(Value: boolean); override;
  public
    procedure Draw; override;
    procedure Hide; override;
    procedure ChangeSize(AScale: double);
    destructor Destroy; override;
    function PointInFugure(aX, aY: integer): boolean; override;
  end;

  TSquare = class(TDot)
  private
    procedure SetSelection(Value: boolean); override;
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

constructor TDot.Create(aX, aY, aColor: integer; ACanvas: TCanvas;
  aSize: integer = 1);
begin
  { �������������� ���� ���������� ������ }
  X := aX;
  Y := aY;
  Canvas := ACanvas;
  Color := aColor;
  Size := aSize;
  Selected := true;
  Draw;
end;

destructor TDot.Destroy;
begin
  { ������ � ������ ����� }
  Selected := False;
  Hide;
  inherited;
end;

procedure TDot.Hide;
var
  TmpColor: integer;
begin
  { �������� ����� � 0-� ������ (�������) }
  TmpColor := Color;
  Color := $FFFFFF;
  Draw;
  Color := TmpColor;
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
    Canvas.Pen.Color := $00FF00
  else
    Canvas.Pen.Color := $FFFFFF;
  Canvas.Rectangle(X - 1, Y - 1, X + 2, Y + 2);
end;

procedure TDot.Shift(aX, aY: integer);
begin
  Hide; { ������ �� ������� ������������ }
  X := aX;
  Y := aY;
  Draw; { �������� � ������ ������������ }
end;

procedure TDot.Draw;
begin
  { ������ ����� }
  Canvas.Pixels[X, Y] := Color;
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

procedure TCircle.Hide;
begin
  inherited;
end;

function TCircle.PointInFugure(aX, aY: integer): boolean;
begin
  result := (((aX - X) * (aX - X) + (aY - Y) * (aY - Y)) <= Size * Size);
end;

procedure TCircle.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    Canvas.Pen.Color := $00FF00
  else
    Canvas.Pen.Color := $FFFFFF;
  Canvas.Ellipse(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Ellipse(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);
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

procedure TSquare.Hide;
begin
  inherited;

end;

function TSquare.PointInFugure(aX, aY: integer): boolean;
begin
  result := not((aX > X + Size) or (aY > Y + Size) or (aX < X - Size) or
      (aY < Y - Size))
end;

procedure TSquare.SetSelection(Value: boolean);
begin
  if Value = FSelected then
    exit;
  FSelected := Value;
  if FSelected then
    Canvas.Pen.Color := $00FF00
  else
    Canvas.Pen.Color := $FFFFFF;
  Canvas.Rectangle(X - Size - 1, Y - Size - 1, X + Size + 1, Y + Size + 1);
  Canvas.Rectangle(X - Size + 1, Y - Size + 1, X + Size - 1, Y + Size - 1);

end;

end.