{$mode objfpc}{$H+}
program dfm_debug_parse;

uses
  SysUtils, Classes, dfm_ast, dfm_parser_debug;

var
  Parser: TDfmParser;
  Doc: TDfmDocument;
  Input: string;
begin
  WriteLn('Debug Parser Test');
  WriteLn('=================');
  WriteLn;
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '  end'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  WriteLn('Parsing with debug output:');
  WriteLn('--------------------------');
  
  Parser := TDfmParser.Create(Input);
  try
    Doc := Parser.Parse;
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn;
      WriteLn('✓ Parse successful!');
      WriteLn('  Root: ', Doc.Root.InstanceName);
      WriteLn('  Children: ', Doc.Root.Children.Count);
    end;
    Doc.Free;
  finally
    Parser.Free;
  end;
end.
