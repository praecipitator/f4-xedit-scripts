{
    TODO update for SS2, refactor, etc
    Run on display SCOLs
}
unit userscript;

uses praUtil;

var
    baseButton, targetFile: IInterface;

// Called before processing
// You can remove it if script doesn't require initialization code
function Initialize: integer;
begin
    Result := 0;
    
    baseButton := FindObjectByEdid('praSim_VendorButton_Base');
    targetFile := GetFile(baseButton);
end;

// called for every record selected in xEdit
function Process(e: IInterface): integer;
var
    newRecord, newRecScript: IInterface;
    newEdid: string;
begin
    Result := 0;
    
    if(signature(e) <> 'SCOL') then begin
        exit;
    end;

    // comment this out if you don't want those messages
    AddMessage('Processing: ' + FullPath(e));

    newEdid := EditorID(e);
    
    newEdid := StringReplace(newEdid, 'praSim_VMD_', 'praSim_VendorButton_', [rfReplaceAll]);

    // processing code goes here
    newRecord := wbCopyElementToFile(baseButton, targetFile, True, True);
    SetElementEditValues(newRecord, 'EDID', newEdid);
    
    
    newRecScript := getScript(newRecord, 'praSim:VendorMachineButtonRef');
    setScriptProp(newRecScript, 'DisplayObject', e);
end;

// Called after processing
// You can remove it if script doesn't require finalization code
function Finalize: integer;
begin
    Result := 0;
end;

end.