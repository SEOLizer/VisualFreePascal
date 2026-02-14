{$mode objfpc}{$H+}
program dfm_parse_only;

uses
  SysUtils, Classes, dfm_ast, dfm_parser;

var
  Parser: TDfmParser;
  Doc: TDfmDocument;
  Input: string;
  I, ChildIdx: Integer;
begin
  WriteLn('Parse-only Test with Child');
  WriteLn('==========================');
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '  end'#13#10 +
           'end';
  
  WriteLn('Input:');
  WriteLn(Input);
  WriteLn;
  
  Parser := TDfmParser.Create(Input);
  try
    WriteLn('Parsing...');
    Doc := Parser.Parse;
    
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn('✓ Parse successful!');
      WriteLn('  Form: ', Doc.Root.InstanceName);
      WriteLn('  Properties: ', Doc.Root.Properties.Count);
      WriteLn('  Children: ', Doc.Root.Children.Count);
      
      for ChildIdx := 0 to Doc.Root.Children.Count - 1 do
      begin
        WriteLn('  Child ', ChildIdx, ': ', TDfmNode(Doc.Root.Children[ChildIdx]).InstanceName);
      end;
    end
    else
    begin
      WriteLn('✗ Parse failed');
    end;
    
    if Assigned(Doc) then Doc.Free;
  finally
    Parser.Free;
  end;
  
  WriteLn('Done.');
end.
