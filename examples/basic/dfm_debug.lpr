{$mode objfpc}{$H+}
program dfm_debug;

uses
  SysUtils, Classes, dfm_ast, dfm_parser;

var
  Parser: TDfmParser;
  Doc: TDfmDocument;
  Input: string;
begin
  WriteLn('DEBUG: Start');
  
  Input := 'object Form1: TForm'#13#10 +
           'end';
  
  WriteLn('DEBUG: Input = ', Input);
  WriteLn('DEBUG: Erstelle Parser...');
  
  Parser := TDfmParser.Create(Input);
  try
    WriteLn('DEBUG: Parse...');
    Doc := Parser.Parse;
    WriteLn('DEBUG: Parse beendet');
    
    if Assigned(Doc) then
    begin
      WriteLn('DEBUG: Doc assigned');
      if Assigned(Doc.Root) then
      begin
        WriteLn('✓ Root gefunden:');
        WriteLn('  Name: ', Doc.Root.InstanceName);
        WriteLn('  Klasse: ', Doc.Root.ClassName);
      end
      else
        WriteLn('✗ Doc.Root ist nil');
      Doc.Free;
    end
    else
      WriteLn('✗ Doc ist nil');
      
  except
    on E: Exception do
      WriteLn('EXCEPTION: ', E.Message);
  end;
  
  Parser.Free;
  WriteLn('DEBUG: Ende');
end.
