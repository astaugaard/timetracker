## GET /

### Response:

- Status code 200
- Headers: []

- No response body

## GET /api/v1/getalldata

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- when there isn't any users (`application/json;charset=utf-8`, `application/json`):

```javascript
{}
```

- normal case (note that users that don't have any timeframes loged don't show up) (`application/json;charset=utf-8`, `application/json`):

```javascript
{"janeSmith":[{"day":60,"lengthOfPeriod":50,"start":5033},{"day":61,"lengthOfPeriod":70,"start":5070}],"johnSmith":[{"day":60,"lengthOfPeriod":50,"start":5033}]}
```

## GET /api/v1/gettodaydata

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- when there isn't any users (`application/json;charset=utf-8`, `application/json`):

```javascript
{}
```

- normal case (note that users that don't have any timeframes loged don't show up) (`application/json;charset=utf-8`, `application/json`):

```javascript
{"janeSmith":[{"lengthOfPeriod":50,"start":5033},{"lengthOfPeriod":70,"start":5070}],"johnSmith":[{"lengthOfPeriod":50,"start":5033}]}
```

## GET /api/v1/loggedin/:email

### Captures:

- *email*: name/email of person to check if logged in

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
false
```

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
true
```

## POST /api/v1/login

### Request:

- Supported content types are:

    - `text/plain;charset=utf-8`

- the general format for passing in the users email is (`text/plain;charset=utf-8`):

```
email:<email>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /api/v1/logout

### Request:

- Supported content types are:

    - `text/plain;charset=utf-8`

- the general format for passing in the users email is (`text/plain;charset=utf-8`):

```
email:<email>
```

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /resource

### Response:

- Status code 200
- Headers: []

- No response body


