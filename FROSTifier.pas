{
    Attempts to automatically figure out VIS tags for selected items and creates overrides for them with those tags.
    Additionally, test files can be put into Edit Scripts\vis-overrides\ in the format of
        editor_id=[VIS TAG] Name
        other_editor_id=Any Name whatsoever
    for manual renaming configuration

}
unit VisGIfier;

    //uses VisOverrides;
    uses praFunctions;
    uses praTagifier;

    var
        ToFile: IInterface;

    procedure initTags();
    begin
        settingAddComponentString := true;

        tagShipment     := '{M4SMT}';       // Shipments
        tagScrap        := '{M4SCP}';       // Scrap, MISCs which contain components
        tagResource     := '{M4RSC}';    // Resources, MISCs which a meant to represent one type of component
        tagLooseMod     := '{M9MOD}';         // Loose modifications
        tagCollectible  := '{M9QST}'; // "Collectible" MISCs
        tagQuest        := '{M9QST}';       // Quest items
        tagCurrency     := '{M2CUR}';    // "Currency", MISCs with zero weight and non-zero value
        tagValuable     := '{M2CUR}';    // MISCs with more value than weight
        tagOtherMisc    := '{M9TRS}';       // all other MISCs, trash
        tagGoodChem     := '(I2AID)';         // Cures etc
        tagBadChem      := '(I3CHM)';        // Addictive chems
        tagFood         := '(I6FOD)';        // generic food, selfcooked. Usually radless
        tagFoodRaw      := '(I6MEA)';    // raw food, has rads, has disease risk
        tagFoodCrop     := '(I6RAW)';     // crops, has rads
        tagFoodPrewar   := '(I6FPW)';    // prewar packaged, has rads
        tagDrink        := '(I3DRK)';       // generic drinkable
        tagLiquor       := '(I4LQR)';      // Alcoholic beverages
        tagNukacola     := '(I3NUK)';        // Nuka Cola of any kind
        tagSyringe      := '[W1AMM]';     // Syringer ammo
        tagDevice       := '(I2DEV)';      // Consumables which are supposed to be devices instead of something to eat, like the Stealth Boy
        tagTool         := '{M4TOL}';        // Similar to above, but for more low-tech things. Like SimSettlements Town Meeting Gavel, or the Companion Whistle
        tagNews         := '{M6NTE}';        // Newspaper
        tagNote         := '{M6NTE}';        // Any paper note
        tagPerkmag      := '{M7PKM}';   // Perk Magazine
        tagMine         := '[W9MNE]';        // Mine
        tagGrenade      := '[W9GND]';     // Grenade
        tagKey          := '{M8KEY}';         // Keycard
        // keyCard         : '{M8PCD}',        // Keycard
        // keyPassword     : '{M8PCD}',        // Password
        tagAmmo         := '[W1AMM]';        // Generic Ammo
        tagHolotape     := '{M6HOL}';    // Holotape
        tagHolotapeGame := '{M6HOL}';
        tagHolotapeSettings := '{M6HOL}';
        tagPipBoy       := '[A0PPB]';
        // If any of the tags in this list, or any of the tags above are discovered (no matter the brackets), it will be considered "tagged", and skipped.
        extraValidTags  := 'A0PPB,A1EYE,A1HAT,A1RNG,A2BPK,A2CLT,A2UAR,A3FAR,A3HZM,A3MSK,A4AHM,A4ARL,A4ARR,A4CHS,A4LLG,A4RLG,A5AHM,A5ARL,A5ARR,A5CHS,A5LLG,A5RLG,A6SMU,A7DOG,I1AID,I1RAD,I1WTR,I2AID,I2BLD,I2DEV,I3CHM,I3DRK,I3NUK,I4LQR,I5FSV,I6FOD,I6FPW,I6MEA,I6RAW,I6WLD,M0TRC,M1BCN,M2CUR,M2LPK,M3CFT,M4BTL,M4RSC,M4SCP,M4SMT,M4TOL,M5CRP,M6HOL,M6NTE,M7PBH,M7PKM,M8KEY,M8PCD,M9MOD,M9QST,M9TRS,M9XST,W1AMM,W1FSC,W2PST,W3REV,W4FLR,W5ASR,W5CBR,W5GRF,W5HMR,W5LER,W5RAR,W5RFL,W5RWR,W5SCR,W5SGN,W5SMG,W5SYR,W6ABL,W6GAM,W6LAS,W6LSM,W6PLA,W7CRY,W7FLM,W7FTM,W7GTL,W7HVY,W7JNK,W7MLR,W7MNG,W8M1H,W8M2H,W8UNA,W9GND,W9MNE,W9SIG,W9TRP';
    end;

    function Initialize: integer;
    var
        dir: String;
        Path: String;
        pattern: String;
        Attr: Integer;
        FileAttr: Integer;
        Res: TSearchRec;
        Name: String;
    begin
        initTagifier();
        initTags();
    end;

    function Process(e: IInterface): integer;
    var
      newElem: IInterface;
    begin
        if Signature(e) = 'TES4' then
            Exit;

        if not Assigned(ToFile) then begin
            ToFile := showFileSelectionDialog(e);

            if not Assigned(ToFile) then begin
                Result := 1;
                Exit;
            end;
        end;

        // now actually do stuff
        newElem := tagifyElement(e, toFile);

    end;

    function Finalize: integer;
    begin
        Result := 0;
        cleanupTagifier();
    end;

end.