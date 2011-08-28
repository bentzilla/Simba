unit accountmanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DOM, XMLWrite, XMLRead, DCPrijndael, DCPsha1;

type

  TStringArrayArray = Array of Array of String;

  { TAccountForm }

  TAccountForm = class(TForm)
    BOX_USER: TLabeledEdit;
    BOX_PASS: TLabeledEdit;
    BOX_PIN: TLabeledEdit;
    BUTTON_SAVE: TButton;
    BUTTON_ADD: TButton;
    BUTTON_DELETE: TButton;
    CHECKBOX_PIN: TCheckBox;
    CHECKBOX_PASS: TCheckBox;
    CHECKBOX_ACTIVE: TCheckBox;
    CHECKBOX_MEMBERS: TCheckBox;
    BOX_NICK: TLabeledEdit;
    BOX_ENCRYPT: TLabeledEdit;
    LIST_ACCOUNTS: TListBox;
    procedure BOX_PINKeyPress(Sender: TObject; var Key: char);
    procedure BOX_USERChange(Sender: TObject);
    procedure BUTTON_ADDClick(Sender: TObject);
    procedure BUTTON_DELETEClick(Sender: TObject);
    procedure BUTTON_SAVEClick(Sender: TObject);
    procedure CHECKBOX_PASSChange(Sender: TObject);
    procedure CHECKBOX_PINChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LIST_ACCOUNTSSelectionChange(Sender: TObject; User: boolean);
    function LIST_ACCOUNTSReturnSelectedIndex: Integer;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AccountForm: TAccountForm;

implementation

//{$R *.lfm}
//uses LCLtype;

{ TAccountForm }

procedure TAccountForm.FormShow(Sender: TObject);
var
  Doc: TXMLDocument;
  i: integer;
  Exists: Boolean;
  s: String;
begin
  try
    if InputQuery('Salt', 'Enter Salt', s) then
      AccountForm.BOX_ENCRYPT.Text := s
    else
      AccountForm.BOX_ENCRYPT.Text := 'SALT';

    Exists := FileExists(ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml');

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml');

    // Load Accounts into List
    for i := 0 to (Doc.DocumentElement.ChildNodes.Count - 1) do
      LIST_ACCOUNTS.Items.Add(Doc.DocumentElement.ChildNodes.Item[i].FirstChild.TextContent);

  finally
    if (Exists) then
      Doc.Free;
  end;
end;

function DoEncrypt(Str, Salt: String): String;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(AccountForm);
  Cipher.InitStr(Salt, TDCP_sha1);
  Result := Cipher.EncryptString(Str);
  Cipher.Burn;
  Cipher.Free;
end;

function DoDecrypt(Str, Salt: String): String;
var
  Cipher: TDCP_rijndael;
begin
  Cipher := TDCP_rijndael.Create(AccountForm);
  Cipher.InitStr(Salt, TDCP_sha1);
  Result := Cipher.DecryptString(Str);
  Cipher.Burn;
  Cipher.Free;
end;

function TAccountForm.LIST_ACCOUNTSReturnSelectedIndex: Integer;
begin
  for Result := 0 to (LIST_ACCOUNTS.Count - 1) do
    if LIST_ACCOUNTS.Selected[Result] then
      Exit;
  Result := -1;
end;

procedure TAccountForm.BOX_PINKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9', #8, #9]) then
    Key := #0;
end;

procedure TAccountForm.BOX_USERChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to (LIST_ACCOUNTS.Count - 1) do
    if LIST_ACCOUNTS.Selected[i] then
      LIST_ACCOUNTS.Items.Strings[i] := BOX_USER.Text;
end;

procedure TAccountForm.LIST_ACCOUNTSSelectionChange(Sender: TObject;
  User: boolean);
var
  Doc: TXMLDocument;
  Exists: Boolean;
begin
  try
    Exists := FileExists(ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml');

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml');

    // New Account
    if (Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex] = nil) then
    begin
      BOX_USER.Text := '';
      BOX_PASS.Text := '';
      BOX_NICK.Text := '';
      BOX_PIN.Text  := '';

      CHECKBOX_MEMBERS.Checked := False;
      CHECKBOX_ACTIVE.Checked  := False;

      Exit;
    end;

    // Load Account info into Boxes
    BOX_USER.Text := Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[0].TextContent;
    BOX_PASS.Text := DoDecrypt(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[1].TextContent, AccountForm.BOX_ENCRYPT.Text);
    BOX_NICK.Text := Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[2].TextContent;
    BOX_PIN.Text  := DoDecrypt(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[3].TextContent, AccountForm.BOX_ENCRYPT.Text);

    CHECKBOX_MEMBERS.Checked := StrToBool(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[4].TextContent);
    CHECKBOX_ACTIVE.Checked  := StrToBool(Doc.DocumentElement.ChildNodes.Item[LIST_ACCOUNTSReturnSelectedIndex].ChildNodes[5].TextContent);

  finally
    if (Exists) then
      Doc.Free;

    BUTTON_DELETE.Enabled := True;
    BUTTON_SAVE.Enabled := True;
    //_________
    BOX_USER.Enabled := True;
    BOX_PASS.Enabled := True;
    BOX_NICK.Enabled := True;
    BOX_PIN.Enabled := True;
    CHECKBOX_MEMBERS.Enabled := True;
    CHECKBOX_ACTIVE.Enabled := True;
  end;
end;

procedure TAccountForm.BUTTON_ADDClick(Sender: TObject);
begin
  LIST_ACCOUNTS.Items.Add('Account #' + IntToStr(LIST_ACCOUNTS.Count));
end;

procedure TAccountForm.BUTTON_DELETEClick(Sender: TObject);
var
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  i, d: integer;
  Exists: Boolean;
  FileName: String;
begin
  try
    FileName := ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml';
    Exists := FileExists(FileName);
    d := LIST_ACCOUNTSReturnSelectedIndex;

    // Load Document
    if (not (Exists)) then
      Exit
    else
      ReadXMLFile(Doc, FileName);

    // Change id #'s

    // Root node
    RootNode := Doc.DocumentElement;

    for i := (d + 1) to (RootNode.ChildNodes.Count - 1) do
    begin
      ElementNode := RootNode.ChildNodes.Item[i];
      if ElementNode = nil then
        Break;
      TDOMElement(ElementNode).SetAttribute('id', IntToStr(i - 1));
    end;

    ElementNode := RootNode.ChildNodes.Item[d];
    if ElementNode = nil then
      Exit;
    RootNode.RemoveChild(ElementNode);
    WriteXMLFile(Doc, FileName);

  finally
    if (Exists) then
      Doc.Free;

    LIST_ACCOUNTS.Items.Delete(d);

    BUTTON_DELETE.Enabled := False;
    BUTTON_SAVE.Enabled := False;
    //___
    BOX_USER.Enabled := False;
    BOX_PASS.Enabled := False;
    BOX_NICK.Enabled := False;
    BOX_PIN.Enabled := False;
    CHECKBOX_MEMBERS.Enabled := False;
    CHECKBOX_ACTIVE.Enabled := False;
  end;
end;

procedure SaveToFile(const FileName: string);
var
  Doc: TXMLDocument;
  RootNode, ElementNode, OldElementNode, ItemNode, TextNode: TDOMNode;
  i: integer;
  Exists: Boolean;
begin
  try
    Exists := FileExists(FileName);
    i := AccountForm.LIST_ACCOUNTSReturnSelectedIndex;

    // Read/Create Document
    if (not (Exists)) then
    begin
      // Create a document
      Doc := TXMLDocument.Create;
      // Create a root node
      RootNode := Doc.CreateElement('Accounts');
      Doc.Appendchild(RootNode);
    end else
      ReadXMLFile(Doc, FileName);

    // Root node
    RootNode := Doc.DocumentElement;

    // Nodes
    if Exists then
      OldElementNode := RootNode.ChildNodes.Item[i];

    ElementNode := Doc.CreateElement('Account');
    TDOMElement(ElementNode).SetAttribute('id', IntToStr(i));

    ItemNode := Doc.CreateElement('Username');
    TextNode := Doc.CreateTextNode(AccountForm.BOX_USER.Text);
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('Password');
    TextNode := Doc.CreateTextNode(DoEncrypt(AccountForm.BOX_PASS.Text, AccountForm.BOX_ENCRYPT.Text));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('Nickname');
    TextNode := Doc.CreateTextNode(AccountForm.BOX_NICK.Text);
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('PIN');
    TextNode := Doc.CreateTextNode(DoEncrypt(AccountForm.BOX_PIN.Text, AccountForm.BOX_ENCRYPT.Text));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('Members');
    TextNode := Doc.CreateTextNode(BoolToStr(AccountForm.CHECKBOX_MEMBERS.Checked, 'True', 'False'));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    ItemNode := Doc.CreateElement('Active');
    TextNode := Doc.CreateTextNode(BoolToStr(AccountForm.CHECKBOX_ACTIVE.Checked, 'True', 'False'));
    ItemNode.AppendChild(TextNode);
    ElementNode.AppendChild(ItemNode);

    //MessageDlg(BoolToStr((OldElementNode = nil), 'T', 'F'),mtError, mbOKCancel, 0);

    if ((OldElementNode = nil) or (not (Exists))) then
      RootNode.AppendChild(ElementNode)
    else
      RootNode.ReplaceChild(ElementNode, OldElementNode);

    // Save XML
    WriteXMLFile(Doc, FileName);

  finally
    if (Exists) then
      Doc.Free;
  end;
end;

procedure TAccountForm.BUTTON_SAVEClick(Sender: TObject);
begin
  if not DirectoryExists(ExtractFileDir(Application.ExeName)) then
    CreateDir(ExtractFileDir(Application.ExeName));
  SaveToFile(ExtractFileDir(Application.ExeName) + DirectorySeparator + 'accounts.xml');
end;

procedure TAccountForm.CHECKBOX_PASSChange(Sender: TObject);
begin
  if CHECKBOX_PASS.Checked then
    BOX_PASS.EchoMode := emNormal
  else
    BOX_PASS.EchoMode := emPassword;
  AccountForm.Refresh;
end;

procedure TAccountForm.CHECKBOX_PINChange(Sender: TObject);
begin
  if CHECKBOX_PIN.Checked then
    BOX_PIN.EchoMode := emNormal
  else
    BOX_PIN.EchoMode := emPassword;
  AccountForm.Refresh;
end;

initialization
  {$R *.lfm}

end.
