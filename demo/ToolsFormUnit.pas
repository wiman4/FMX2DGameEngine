unit ToolsFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TForm2 = class(TForm)
    mmo1: TMemo;
    btnReverse: TButton;
    procedure btnReverseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private    { Private declarations }
  public    { Public declarations }
  end;

var
  Form2: TForm2;


implementation

uses
  System.Math.Vectors, System.StrUtils, gameSpriteUnit, System.Generics.Collections;

{$R *.fmx}

procedure TForm2.btnReverseClick(Sender: TObject);
var
  bmp, bmp1: TBitmap;
var
  mat0, mat: TMatrix;
  rt, rt2: TRectF;
  fs, fs1: TFileStream;
  frms: TMemoryStream;
  k, i: Integer;
  s, s1: string;

  cnt: Integer;
  frame: TGameSprFrame;
  rt0: TRect;
begin

  for k := 0 to mmo1.Lines.Count - 1 do
  begin
    s := mmo1.Lines[k];
    bmp := TBitmap.CreateFromFile(s);
    fs := TFileStream.Create(s, fmOpenRead);
    bmp1 := TBitmap.Create(bmp.Width, bmp.Height);
    bmp1.Canvas.BeginScene;
    mat0 := bmp1.Canvas.Matrix;


    fs.Seek(-20, 2);
    fs.Read(i, 4);
    fs.Seek(-20 - i - 8, 2);
    frms := TMemoryStream.Create;
    frms.SetSize(Int64(20 + 8 + i));
    fs.Read(frms.Memory^, frms.Size);

    frms.Seek(16, 0);
    frms.Read(cnt, 4);  //allFrameCnt
    for i := 1 to cnt do
    begin
      frms.Read(rt0, SizeOf(rt0));
      rt := rt0;
      frms.Seek(8, 1);
      mat := TMatrix.CreateScaling(-1, 1) * TMatrix.CreateTranslation(rt.Left + rt.Right, 0);
      bmp1.Canvas.SetMatrix(mat);
      bmp1.Canvas.DrawBitmap(bmp, rt, rt, 1);
    end;

    bmp1.Canvas.SetMatrix(mat0);
    bmp1.Canvas.EndScene;
    s1 := ExtractFilePath(s);
    s1 := s1 + 'left' + ExtractFileExt(s);
    fs1 := TFileStream.Create(s1, fmCreate);
    bmp1.SaveToStream(fs1);

    fs1.Seek(-12, 2);
    fs1.Write(frms.Memory^, frms.Size);
    fs1.Free;
    fs.Free;
    frms.Free;
    bmp.Free;
    bmp1.Free;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
//var
//  mp: TMapPoint;
//  lst: TMapPtList;
begin
  {$IFDEF mswindows}
//  Show;
//  InitTownIni;
  {$endif}

//  lst := TMapPtList.Create;
//  SearchPath(1101, 1431, lst);
//  for mp in lst do
//  begin
//    mmo1.Lines.Add(Format('%d,%d %d', [mp.mapPos[0].pos.X, mp.mapPos[0].pos.Y, mp.NextMapId]));
//  end;
//  lst.Free;
//  mp.NextMapId := 1431;
//  GetGotoString(1101, mp, mmo1.Lines);
end;

end.

