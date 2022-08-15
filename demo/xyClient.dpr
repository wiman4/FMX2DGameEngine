program xyClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  ToolsFormUnit in 'ToolsFormUnit.pas' {Form2},
  richTextUnit in 'richTextUnit.pas',
  MeiStream in '..\pub\MeiStream.pas',
  pubUnit in '..\pub\pubUnit.pas',
  stringUnit in '..\pub\stringUnit.pas',
  TypeUnit in '..\pub\TypeUnit.pas',
  gameAniUnit in '..\src\gameAniUnit.pas',
  gameSpriteUnit in '..\src\gameSpriteUnit.pas',
  UIunit in '..\src\UIunit.pas',
  MeiPack in '..\src\MeiPack.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
