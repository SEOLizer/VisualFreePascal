{$mode objfpc}{$H+}
program dfm_step_test;

uses
  SysUtils, Classes, dfm_ast, dfm_parser, dfm_writer;

procedure Test1_Simple;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
begin
  WriteLn('=== Test 1: Simple Form ===');
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 100'#13#10 +
           '  Top = 200'#13#10 +
           'end';
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    Doc := Parser.ParseText(Input);
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      Output := Writer.WriteDocument(Doc);
      WriteLn('✓ Success');
      WriteLn('Root: ', Doc.Root.InstanceName);
      WriteLn('Output:');
      WriteLn(Output);
    end
    else
      WriteLn('✗ Parse failed');
    Doc.Free;
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

procedure Test2_WithChild;
var
  Parser: TDfmParser;
  Writer: TDfmWriter;
  Doc: TDfmDocument;
  Input, Output: string;
  I, J: Integer;
begin
  WriteLn('=== Test 2: Form with Button ===');
  
  Input := 'object Form1: TForm'#13#10 +
           '  Left = 0'#13#10 +
           '  object Button1: TButton'#13#10 +
           '    Left = 10'#13#10 +
           '  end'#13#10 +
           'end';
  
  Parser := TDfmParser.Create;
  Writer := TDfmWriter.Create;
  try
    Doc := Parser.ParseText(Input);
    if Assigned(Doc) and Assigned(Doc.Root) then
    begin
      WriteLn('✓ Success');
      WriteLn('Root: ', Doc.Root.InstanceName);
      WriteLn('Children: ', Doc.Root.Children.Count);
      
      for J := 0 to Doc.Root.Children.Count - 1 do
        WriteLn('  Child: ', TDfmNode(Doc.Root.Children[J]).InstanceName);
      
      Output := Writer.WriteDocument(Doc);
      WriteLn('Output:');
      WriteLn(Output);
      Doc.Free;
    end
    else
      WriteLn('✗ Parse failed');
  finally
    Parser.Free;
    Writer.Free;
  end;
end;

begin
  WriteLn('Step by Step DFM Test');
  WriteLn('=====================');
  WriteLn;
  
  try
    Test1_Simple;
    WriteLn;
    Test2_WithChild;
    WriteLn;
    WriteLn('All tests passed!');
  except
    on E: Exception do
      WriteLn('ERROR: ', E.Message);
  end;
end.
