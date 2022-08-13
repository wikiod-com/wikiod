---
title: "PackageManager"
slug: "packagemanager"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Retrieve application version
    public String getAppVersion() throws PackageManager.NameNotFoundException {
        PackageManager manager = getApplicationContext().getPackageManager();
        PackageInfo info = manager.getPackageInfo(
                                getApplicationContext().getPackageName(), 
                                0);

        return info.versionName;
    }

## Version name and version code


## Install time and update time


## Utility method using PackageManager
Here we can find some useful method using PackageManager,

*Below method will help to get the app name using package name*

    private String getAppNameFromPackage(String packageName, Context context) {
        Intent mainIntent = new Intent(Intent.ACTION_MAIN, null);
        mainIntent.addCategory(Intent.CATEGORY_LAUNCHER);
        List<ResolveInfo> pkgAppsList = context.getPackageManager()
                .queryIntentActivities(mainIntent, 0);
        for (ResolveInfo app : pkgAppsList) {
            if (app.activityInfo.packageName.equals(packageName)) {
                return app.activityInfo.loadLabel(context.getPackageManager()).toString();
            }
        }
        return null;
    }

*Below method will help to get the app icon using package name,*

    private Drawable getAppIcon(String packageName, Context context) {
        Drawable appIcon = null;
        try {
            appIcon = context.getPackageManager().getApplicationIcon(packageName);
        } catch (PackageManager.NameNotFoundException e) {
        }

        return appIcon;
    }

*Below method will help to get the list of installed application.*

    public static List<ApplicationInfo> getLaunchIntent(PackageManager packageManager) {

        List<ApplicationInfo> list = packageManager.getInstalledApplications(PackageManager.GET_META_DATA);

        return list;
    }

> Note: above method will give the launcher application too.

*Below method will help to hide the app icon from the launcher.*

    public static void hideLockerApp(Context context, boolean hide) {
        ComponentName componentName = new ComponentName(context.getApplicationContext(),
                SplashActivity.class);

        int setting = hide ? PackageManager.COMPONENT_ENABLED_STATE_DISABLED
                : PackageManager.COMPONENT_ENABLED_STATE_ENABLED;

        int current = context.getPackageManager().getComponentEnabledSetting(componentName);

        if (current != setting) {
            context.getPackageManager().setComponentEnabledSetting(componentName, setting,
                    PackageManager.DONT_KILL_APP);
        }
    }

> Note: After switch off the device and switch on this icon will come
> back in the launcher.

