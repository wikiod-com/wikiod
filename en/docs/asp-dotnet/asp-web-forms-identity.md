---
title: "Asp Web Forms Identity"
slug: "asp-web-forms-identity"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Getting Started
[Getting Started](https://docs.microsoft.com/en-us/aspnet/identity/overview/getting-started/introduction-to-aspnet-identity)

  **Install NuGet packages:** 

1. **Microsoft.AspNet.Identity.EntityFramework**

2. **Microsoft.AspNet.Identity.Core**

3. **Microsoft.AspNet.Identity.OWIN**

**Register action** - Account controller

    [HttpPost]
    [AllowAnonymous]
    [ValidateAntiForgeryToken]
    public async Task<ActionResult> Register(RegisterViewModel model)
    {
        if (ModelState.IsValid)
        {
            var user = new ApplicationUser() { UserName = model.UserName };
            var result = await UserManager.CreateAsync(user, model.Password);
            if (result.Succeeded)
            {
                await SignInAsync(user, isPersistent: false);
                return RedirectToAction("Index", "Home");
            }
            else
            {
                AddErrors(result);
            }
        }
    
        // If we got this far, something failed, redisplay form
        return View(model);
    }


**Log-in action** - SignInAsync method

    private async Task SignInAsync(ApplicationUser user, bool isPersistent)
    {
        AuthenticationManager.SignOut(DefaultAuthenticationTypes.ExternalCookie);
    
        var identity = await UserManager.CreateIdentityAsync(
           user, DefaultAuthenticationTypes.ApplicationCookie);
    
        AuthenticationManager.SignIn(
           new AuthenticationProperties() { 
              IsPersistent = isPersistent 
           }, identity);
    }

**Log off**

    // POST: /Account/LogOff
    [HttpPost]
    [ValidateAntiForgeryToken]
    public ActionResult LogOff()
    {
        AuthenticationManager.SignOut();
        return RedirectToAction("Index", "Home");
    }

