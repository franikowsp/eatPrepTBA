# login works as inteded [plain]

    Code
      createTestcenterLogin()
    Message
      v Login was successful.
      A token was generated to access the following workspaces (id: label):
      * 134: eatPrepTBA Documentation
      i Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [ansi]

    Code
      createTestcenterLogin()
    Message
      [32mv[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      * [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mi[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [unicode]

    Code
      createTestcenterLogin()
    Message
      ✔ Login was successful.
      A token was generated to access the following workspaces (id: label):
      • 134: eatPrepTBA Documentation
      ℹ Please note that the login becomes invalid if you log in to the Testcenter manually.

# login works as inteded [fancy]

    Code
      createTestcenterLogin()
    Message
      [32m✔[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      • [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mℹ[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

# login fails with wrong or omitted name and/or password [plain]

    Code
      createTestcenterLogin(name = "eatPrepTBA", password = "")
    Message
      v Login was successful.
      A token was generated to access the following workspaces (id: label):
      * 134: eatPrepTBA Documentation
      i Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "eatPrepTBA")
    Message
      v Login was successful.
      A token was generated to access the following workspaces (id: label):
      * 134: eatPrepTBA Documentation
      i Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "")
    Message
      v Login was successful.
      A token was generated to access the following workspaces (id: label):
      * 134: eatPrepTBA Documentation
      i Please note that the login becomes invalid if you log in to the Testcenter manually.

# login fails with wrong or omitted name and/or password [ansi]

    Code
      createTestcenterLogin(name = "eatPrepTBA", password = "")
    Message
      [32mv[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      * [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mi[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "eatPrepTBA")
    Message
      [32mv[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      * [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mi[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "")
    Message
      [32mv[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      * [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mi[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

# login fails with wrong or omitted name and/or password [unicode]

    Code
      createTestcenterLogin(name = "eatPrepTBA", password = "")
    Message
      ✔ Login was successful.
      A token was generated to access the following workspaces (id: label):
      • 134: eatPrepTBA Documentation
      ℹ Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "eatPrepTBA")
    Message
      ✔ Login was successful.
      A token was generated to access the following workspaces (id: label):
      • 134: eatPrepTBA Documentation
      ℹ Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "")
    Message
      ✔ Login was successful.
      A token was generated to access the following workspaces (id: label):
      • 134: eatPrepTBA Documentation
      ℹ Please note that the login becomes invalid if you log in to the Testcenter manually.

# login fails with wrong or omitted name and/or password [fancy]

    Code
      createTestcenterLogin(name = "eatPrepTBA", password = "")
    Message
      [32m✔[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      • [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mℹ[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "eatPrepTBA")
    Message
      [32m✔[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      • [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mℹ[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

---

    Code
      createTestcenterLogin(name = "", password = "")
    Message
      [32m✔[39m Login was successful.
      A token was generated to access the following workspaces ([90mid[39m: [90mlabel[39m):
      • [90m134[39m: [90meatPrepTBA Documentation[39m
      [36mℹ[39m Please note that the login becomes invalid if you log in to the Testcenter manually.

