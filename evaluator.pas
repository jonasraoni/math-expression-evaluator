{*
 * Evaluator: Math expression evaluator written in Pascal using pointers.
 * Jonas Raoni Soares da Silva <http://raoni.org>
 * https://github.com/jonasraoni/math-expression-evaluator
 *}

unit evaluator;

interface

uses
  dialogs, SysUtils, Classes, Math;

const
  OPERATORS = '^~:*/:%:+-';

type
  TTokenType = ( ttNoToken, ttNumber, ttSignal, ttParen );
  TToken = record
    offset: pchar;
    content: pchar;
    length: integer;
    token: TTokenType;
  end;

function greaterSignal( const a, b: char ): boolean;
function operate( const left, right: extended; const operator: char ): extended;
function getNumber( offset: pchar ): integer;
function getParen( offset: pchar ): integer;
function nextToken( offset: pchar ): TToken;
procedure freeToken( var t: TToken );
function getString( offset: pchar; const length: integer ): pchar;
function evalExpression( expression: pchar ): extended;
function floatToChar( value: extended ): pchar;


implementation

function floatToChar( value: extended ): pchar;
var
  i: integer;
begin
  getMem( result, 65 );
  i := floatToText( result, value, fvExtended, ffGeneral, 18, 0 );
  reallocMem( result, i+1 );
  (result+i)^ := #0;
end;

function greaterSignal( const a, b: char ): boolean;
var
  posA, posB, l, h: integer;
begin
  result := false;
  posA := pos( a, OPERATORS );
  posB := pos( b, OPERATORS );
  l := min( posA , posB );
  h := max( posA, posB ) - 1;
  while l < h do begin
    inc( l );
      if OPERATORS[l] = ':' then begin
          result := h + 1 = posB;
          break;
      end;
  end;
end;

function operate( const left, right: extended; const operator: char ): extended;
begin
  case operator of
    '^': result := power( left, right );
    '~':
      if right = 0 then
        raise exception.create( 'Não existe raiz de 0' )
      else
        result := power( left, 1 / right );
    '*': result := left * right;
    '/':
      if right = 0 then
        raise exception.create( 'Não existe divisão por 0' )
      else
        result := left / right;
    '%': result := trunc( left ) mod trunc( right );
    '+': result := left + right;
    '-': result := left - right;
  else
    raise exception.create( 'operador desconhecido.' );
  end;
end;

function getNumber( offset: pchar ): integer;
begin
  result := integer( offset );
  while ( ( offset^ >= '0' ) and ( offset^ <= '9' ) ) or ( offset^ = DecimalSeparator ) do
    inc( offset );
  result := integer( offset ) - result;
end;

function getParen( offset: pchar ): integer;
var
  i: integer;
begin
  result := integer( offset );
  i := 1;
  inc( offset );
  while offset^ <> #0 do begin
    case offset^ of
      '(': inc( i );
      ')': begin
        dec( i );
        if i = 0 then
          break;
      end;
    end;
    inc( offset );
  end;
  result := integer( offset ) - result;
end;

procedure freeToken( var t: TToken );
begin
  freeMem( t.content );
end;

function nextToken( offset: pchar ): TToken;

  procedure setResult( sign: TTokenType; const len: integer );
  begin
    result.length := len;
    result.offset := offset;
    result.content := getString( offset, len );
    result.token := sign;
  end;

begin
  while offset^ <> #0 do begin
    if ( offset^ >= '0' ) and ( offset^ <= '9' ) then begin
      setResult( ttNumber, getNumber( offset ) );
      exit;
    end
    else if pos( offset^ , OPERATORS ) <> 0 then begin
      setResult( ttSignal, 1 );
      exit;
    end
    else if offset^ = '(' then begin
      inc( offset );
      setResult( ttParen, getParen( offset ) );
      exit;
    end;
    inc( offset );
  end;
  setResult( ttNoToken, 0 );
end;

function getString( offset: pchar; const length: integer ): pchar;
begin
  getMem( result, length + 1 );
  strLCopy( result, offset, length );
end;

function evalExpression( expression: pchar ): extended;
var
  x: TToken;
  exp, expBegin: pchar;

  function read: TTokenType;
  begin
    x := nextToken( exp );
    with x do begin
      exp := offset + length;
      result := token;
    end;
  end;

  function getNumber: extended;
  begin
    result := 0;
    case x.token of
      ttNumber: result := strToFloat( x.content );
      ttParen: result := evalExpression( x.content );
      ttSignal:
        if x.content^ = '-' then begin
          freeToken( x );
          read;
          result := getNumber * -1;
        end
        else if x.content^ = '+' then begin
          freeToken( x );
          read;
          result := getNumber;
        end;
    end;
  end;

var
  left, right: extended;
  op: char;
  tempExp, remainingExp: pchar;
begin
  getMem( exp, strLen( expression ) + 1 );
  exp := strCopy( exp, expression );
  expBegin := exp;
  result := 0;
  while exp^ <> #0 do begin
    remainingExp := nil;
    if read = ttNoToken then begin
      freeToken( x );
      break;
    end;
    left := getNumber;
    freeToken( x );
    if read <> ttSignal then begin
      freeToken( x );
      result := left;
      break;
    end;
    op := x.content^;
    freeToken( x );
    if read = ttNoToken then begin
      freeToken( x );
      freeMem( expBegin );
      raise exception.create( 'erro! operando direito esperado' );
    end;
    right := getNumber;
    freeToken( x );
    if read <> ttSignal then begin
      freeToken( x );
      result := result + operate( left, right, op );
      break;
    end
    else if not greaterSignal( x.content^ , op ) then begin
      tempExp := floatToChar( operate( left, right, op ) );
      strCopy( expBegin, x.offset );
      reallocMem( expBegin, strLen( tempExp ) + strLen( expBegin ) + 1 );
      exp := strMove( expBegin + strLen( tempExp ), expBegin, strLen( expBegin ) + 1 );
      exp := strMove( expBegin, tempExp, strLen( tempExp ) ); //exp - expBegin
      freeMem( tempExp );
      freeToken( x );
    end
    else begin
      remainingExp := allocMem( 1 );
      repeat
        tempExp := floatToChar( left );
        reallocMem( remainingExp, strLen( remainingExp ) + strLen( tempExp ) + {strLen( @op )} 1 + 1 );
        strCat( remainingExp, tempExp );
        StrLCat( remainingExp, @op, strLen( remainingExp ) + 1 );
        freeMem( tempExp );
        left := right;
        op := x.content^;
        freeToken( x );
        if read = ttNoToken then begin
          freeToken( x );
          freeMem( remainingExp );
          freeMem( expBegin );
          raise exception.Create( 'erro! operando direito esperado' );
        end;
        right := getNumber;
        freeToken( x );
      until ( read <> ttSignal ) or not greaterSignal( x.content^, op );

      tempExp := floatToChar( operate( left, right, op ) );
      reallocMem( remainingExp, strLen( remainingExp ) + strlen( tempExp ) + strlen( x.offset ) + 1 );
      strCat( strCat( remainingExp, tempExp ), x.offset );

      freeMem( expBegin );
      expBegin := remainingExp;
      exp := expBegin;

      freeMem( tempExp );
      freeToken( x );
    end;
  end;
  freeMem( expBegin );
end;

end.
