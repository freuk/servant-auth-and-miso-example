## GET /

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/html;charset=utf-8`

- Response body as below.

```html
&lt;html&gt;&lt;/html&gt;
```

## GET /die/roll

### Authentication

This part of the API is protected by [Cookies](https://en.wikipedia.org/wiki/HTTP_cookie)


Clients must supply the following data
Cookies automatically set by browsers, plus a header


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
1
```

## GET /loggedin

### Authentication

This part of the API is protected by [Cookies](https://en.wikipedia.org/wiki/HTTP_cookie)


Clients must supply the following data
Cookies automatically set by browsers, plus a header


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"userId":1,"userEmail":"foo@bar"}
```

## POST /login

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
{"username":"foo","password":"bar"}
```

### Response:

- Status code 204
- Headers: [("Set-Cookie","cookieName=cookieValue"),("Set-Cookie","cookieName=cookieValue")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript

```

## GET /static

### Response:

- Status code 200
- Headers: []

- No response body

