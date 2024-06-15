{
    An attempt to implement some of the missing math unit functions for xEdit pascal.
    Algorithms taken from here: http://web.archive.org/web/20220706091202/http://mathonweb.com/help_ebook/html/algorithms.htm
}
unit XeditSimpleMath;

    const
        MATH_PI = 3.1415926535897932;
        MATH_HALF_PI = MATH_PI / 2.0;
        MATH_QUARTER_PI = MATH_PI / 4.0;
        MATH_TWO_PI = 2.0 * MATH_PI;
        MATH_THREE_HALVES_PI = MATH_PI * 3.0/2.0;


    /////////////// HELPERS ///////////////


    // Returns true if `value1` is within `margin` of `value2`
    function isWithin(value1, value2, margin: float): boolean;
    begin
        Result := AbsoluteValue(value1-value2) <= margin;
    end;


    // Returns the absolute value
    function AbsoluteValue(val: float): float;
    begin
        if(val < 0) then begin
            Result := val * -1;
            exit;
        end;

        Result := val;
    end;

    /////////////// ANGLE NORMALIZERS ///////////////
    {
        Normalize rad
    }
    function normalizeAngle(rad: float): float;
    begin
        while (rad < 0) do begin
            rad := rad + MATH_TWO_PI;
        end;

        while (rad > MATH_TWO_PI) do begin
            rad := rad - MATH_TWO_PI;
        end;

        Result := rad;
    end;

    {
        Normalize deg
    }
    function normalizeAngleDeg(deg: float): float;
    begin
        while (deg < 0) do begin
            deg := deg + 360;
        end;

        while (deg > 360) do begin
            deg := deg - 360;
        end;

        Result := deg;
    end;

    /////////////// DEG<-->RAD CONVERTERS ///////////////

    {
        Deg to Rad
    }
    function DegToRad(degrees: float): float;
    begin
         Result := degrees / 180.0 * MATH_PI;
    end;

    {
        Rad to Deg
    }
    function RadToDeg(rad: float): float;
    begin
         Result := rad * 180.0 / MATH_PI;
    end;

    /////////////// REVERSE FUNCTIONS ///////////////
    {
        Arcsin
    }
    function arcsin(x: float): float;
    begin
        Result := asinReal(x);
    end;

    function asinReal(x: float): float;
    begin
        if (x = 1) then begin
            Result := MATH_HALF_PI;
            exit;
        end;

        if (x = -1) then begin
            Result := MATH_THREE_HALVES_PI;
            exit;
        end;

        Result := arctan( x / sqrt(1-power(x, 2))  );
    end;


    {
        Arccos
    }
    function arccos(x: float): float;
    begin
        Result := acosReal(x);
    end;

    function acosReal(x: float): float;
    begin
        if(x = 0) then begin
            Result := MATH_HALF_PI;
            exit;
        end;

        if(x < 0) then begin
            Result := MATH_PI - acosReal(x * -1);
            exit;
        end;

        Result := arctan( sqrt(1-power(x,2)) / x );
    end;

    /////////////// SYNONYMS ///////////////
    function asin(x: float): float;
    begin
        Result := arcsin(x);
    end;

    function acos(x: float): float;
    begin
        Result := arccos(x);
    end;

    function atan(x: float): float;
    begin
        Result := arctan(x); // that exists in xEdit
    end;


    /////////////// Deg-based functions ///////////////
    function sinDeg(x: float): float;
    begin
        Result := sin(DegToRad(x));
    end;

    function cosDeg(x: float): float;
    begin
        Result := cos(DegToRad(x));
    end;

    function tanDeg(x: float): float;
    begin
        Result := tan(DegToRad(x));
    end;

    function asinDeg(x: float): float;
    begin
        Result := RadToDeg(arcsin(x));
    end;

    function acosDeg(x: float): float;
    begin
        Result := RadToDeg(arccos(x));
    end;

    function atanDeg(x: float): float;
    begin
        Result := RadToDeg(arctan(x));
    end;
end.