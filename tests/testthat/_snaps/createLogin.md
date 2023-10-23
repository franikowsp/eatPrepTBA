# login works as inteded [plain]

    Code
      createLogin()
    Message <cliMessage>
      v Login was successful.
      A token was generated to access the following workspaces (id: label):
      * 134: eatPrepTBA Documentation
      i Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [ansi]

    Code
      createLogin()
    Message <cliMessage>
      [32mv[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      * [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mi[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [unicode]

    Code
      createLogin()
    Message <cliMessage>
      ✔ Login was successful.
      A token was generated to access the following workspaces (id: label):
      • 134: eatPrepTBA Documentation
      ℹ Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [fancy]

    Code
      createLogin()
    Message <cliMessage>
      [32m✔[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      • [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mℹ[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

# login fails with wrong or omitted name and/or password [plain]

    Code
      createLogin(name = "eatPrepTBA", password = "")
    Message <cliMessage>
      x Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "eatPrepTBA")
    Message <cliMessage>
      x Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "")
    Message <cliMessage>
      x Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

# login fails with wrong or omitted name and/or password [ansi]

    Code
      createLogin(name = "eatPrepTBA", password = "")
    Message <cliMessage>
      [31mx[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "eatPrepTBA")
    Message <cliMessage>
      [31mx[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "")
    Message <cliMessage>
      [31mx[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

# login fails with wrong or omitted name and/or password [unicode]

    Code
      createLogin(name = "eatPrepTBA", password = "")
    Message <cliMessage>
      ✖ Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "eatPrepTBA")
    Message <cliMessage>
      ✖ Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "")
    Message <cliMessage>
      ✖ Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

# login fails with wrong or omitted name and/or password [fancy]

    Code
      createLogin(name = "eatPrepTBA", password = "")
    Message <cliMessage>
      [31m✖[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "eatPrepTBA")
    Message <cliMessage>
      [31m✖[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

---

    Code
      createLogin(name = "", password = "")
    Message <cliMessage>
      [31m✖[39m Login was not successful. Please check if you have admin rights or are already logged in on a browser.
    Output
      NULL

