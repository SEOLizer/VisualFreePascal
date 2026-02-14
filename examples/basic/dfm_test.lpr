{$mode objfpc}{$H+}
program dfm_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser;

var
  Parser: TDfmParser;
  Doc: TDfmDocument;
  Input: string;
begin
  WriteLn('DFM Parser Test');
  WriteLn('===============');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 100'#13#10 +
           '  Top = 200'#13#10 +
           'end';
  
  WriteLn('Input DFM:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    Doc := Parser.Parse;
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn('✓ Parse erfolgreich!');
      WriteLn('  Name: ', Doc.Root.InstanceName);
      WriteLn('  Klasse: ', Doc.Root.ClassName);
    end
    else
    begin
      WriteLn('✗ Parse fehlgeschlagen');
    end;
  finally
    Parser.Free;
  end;
end.
