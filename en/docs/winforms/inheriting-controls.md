---
title: "Inheriting Controls"
slug: "inheriting-controls"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

Controls are derived in exactly the same way as other classes.  The only thing to be careful of is overriding events: it is usually advisable to make sure that you call the base event handler after your own.  My own rule of thumb: if in doubt, call the base event.

## Application wide Settings
A quick read of most developer sites will reveal that WinForms is considered inferior to WPF.  One of the most often cited reasons is the supposed difficulty in making application wide changes to the "look-and-feel" of an entire application.

In fact it is surprisingly easy to produce an application in WinForms that is easily configurable both at design-time and run-time, if you simply eschew the use of the standard controls and derive your own from them.

Take the TextBox as an example.  It is hard to imagine a Windows application that does not call for the use of a TextBox at some stage or other.  Therefore, having your own TextBox will always make sense.  Take the following example:

    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Windows.Forms;
    
    namespace StackOverflowDocumentation
    {
        public class SOTextBox : TextBox
        {
            public SOTextBox() : base()
            {
                base.BackColor = SOUserPreferences.BackColor;
                base.ForeColor = SOUserPreferences.ForeColor;
            }
            protected override void OnEnter(EventArgs e)
            {
                base.BackColor = SOUserPreferences.FocusColor;
                base.OnEnter(e);
            }
            protected override void OnLeave(EventArgs e)
            {
                base.BackColor = SOUserPreferences.BackColor;
                base.OnLeave(e);
            }
        }
    }

One of the things that users find most helpful in a data entry form, with many input boxes, is to have the background colour of the box with focus change.  Visibly it is easier to see, than a standard blinking vertical cursor.  The above code provides a TextBox that does precisely that.  

In the process it makes use of the static properties of a static class.  I give below an extract from mine:


    using System;
    using System.Threading;
    using Microsoft.Win32;
    using System.Globalization;
    using System.Data;
    using System.Drawing;
    
    namespace StackOverflowDocumentation
    {
        public class SOUserPreferences
        {
            private static string language;
            private static string logPath;
            private static int formBackCol;
            private static int formForeCol;
            private static int backCol;
            private static int foreCol;
            private static int focusCol;

            static SOUserPreferences()
            {
                try
                {
                    RegistryKey HKCU = Registry.CurrentUser;
                    RegistryKey kSOPrefs = HKCU.OpenSubKey("SOPrefs");
                    if (kSOPrefs != null)
                    {
                        language = kSOPrefs.GetValue("Language", "EN").ToString();
                        logPath = kSOPrefs.GetValue("LogPath", "c:\\windows\\logs\\").ToString();
                        formForeCol = int.Parse(kSOPrefs.GetValue("FormForeColor", "-2147483630").ToString());
                        formBackCol = int.Parse(kSOPrefs.GetValue("FormBackColor", "-2147483633").ToString());
                        foreCol = int.Parse(kSOPrefs.GetValue("ForeColor", "-2147483640").ToString());
                        backCol = int.Parse(kSOPrefs.GetValue("BackColor", "-2147483643").ToString());
                        focusCol = int.Parse(kSOPrefs.GetValue("FocusColor", "-2147483643").ToString());
                    }
                    else
                    {
                        language = "EN";
                        logPath = "c:\\windows\\logs\\";
                        formForeCol = -2147483630;
                        formBackCol = -2147483633;
                        foreCol = -2147483640;
                        backCol = -2147483643;
                        focusCol = -2147483643;
                    }
                }
                catch (Exception ex)
                {
                    //handle exception here;
                }
            }
            
            public static string Language
            {
                get
                {
                    return language;
                }
                set
                {
                    language = value;
                }
            }

            public static string LogPath
            {
                get
                {
                    return logPath;
                }
                set
                {
                    logPath = value;
                }
            }

            public static Color FormBackColor
            {
                get
                {
                    return ColorTranslator.FromOle(formBackCol);
                }
                set
                {
                    formBackCol = ColorTranslator.ToOle(value);
                }
            }

            public static Color FormForeColor
            {
                get
                {
                    return ColorTranslator.FromOle(formForeCol);
                }
                set
                {
                    formForeCol = ColorTranslator.ToOle(value);
                }
            }

            public static Color BackColor
            {
                get
                {
                    return ColorTranslator.FromOle(backCol);
                }
                set
                {
                    backCol = ColorTranslator.ToOle(value);
                }
            }

            public static Color ForeColor
            {
                get
                {
                    return ColorTranslator.FromOle(foreCol);
                }
                set
                {
                    foreCol = ColorTranslator.ToOle(value);
                }
            }

            public static Color FocusColor
            {
                get
                {
                    return ColorTranslator.FromOle(focusCol);
                }
                set
                {
                    focusCol = ColorTranslator.ToOle(value);
                }
            }
        }
    }

This class uses the Windows registry to persist the properties, but you can use a database or a settings file if you prefer.  The advantage of using a static class in this way is that application wide changes can be made not only at design-time, but also by the user at run-time.  I always include a form in my applications allowing the user to change the preferred values.  The save function not only saves to the Registry (or database etc), but it also at run-time changes the propeties in the static class.  Note that static properties of a static class are not constant; in this sense they may be regarded as application wide variables.  This means that any form opened subsequent to the changes being saved will immediately be affected by any changes saved.

You will easily be able to think of other application wide properties that you would like to be configurable in the same way.  Fonts are another very good example.  






## NumberBox
Often you will want to have an input box that takes numbers only.  Again by deriving from the standard controls this is easily achieved, for example:

    using System;
    using System.Windows.Forms;
    using System.Globalization;
    
    namespace StackOverflowDocumentation
    {
        public class SONumberBox : SOTextBox
        {
            private int decPlaces;
            private int extraDecPlaces;
            private bool perCent;
            private bool useThouSep = true;
            private string decSep = ".";
            private string thouSep = ",";
            private double numVal;
    
            public SONumberBox() : base()

        {
        }

        public bool PerCent
        {
            get
            {
                return perCent;
            }
            set
            {
                perCent = value;
            }
        }

        public double Value
        {
            get
            {
                return numVal;
            }
            set
            {
                numVal = value;
                if (perCent)
                {
                    double test = numVal * 100.0;
                    this.Text = FormatNumber(test) + "%";
                }
                else
                {
                    this.Text = FormatNumber(value);
                }
            }
        }
        public bool UseThousandSeparator
        {
            get
            {
                return useThouSep;
            }
            set
            {
                useThouSep = value;
            }
        }
        public int DecimalPlaces
        {
            get
            {
                return decPlaces;
            }
            set
            {
                decPlaces = value;
            }
        }
        public int ExtraDecimalPlaces
        {
            get
            {
                return extraDecPlaces;
            }
            set
            {
                extraDecPlaces = value;
            }
        }
        protected override void OnTextChanged(EventArgs e)
        {
            string newVal = this.Text;
            int len = newVal.Length;
            if (len == 0)
            {
                return;
            }
            bool neg = false;
            if (len > 1)
            {
                if (newVal.Substring(0, 1) == "-")
                {
                    newVal = newVal.Substring(1, len - 1);
                    len = newVal.Length;
                    neg = true;
                }
            }
            double val = 1.0;
            string endChar = newVal.Substring(newVal.Length - 1);
            switch (endChar)
            {
                case "M":
                case "m":
                    if (len > 1)
                    {
                        val = double.Parse(newVal.Substring(0, len - 1)) * 1000000.0;
                    }
                    else
                    {
                        val *= 1000000.0;
                    }
                    if (neg)
                    {
                        val = -val;
                    }
                    this.Text = FormatNumber(val);
                    break;
                case "T":
                case "t":
                    if (len > 1)
                    {
                        val = double.Parse(newVal.Substring(0, len - 1)) * 1000.0;
                    }
                    else
                    {
                        val *= 1000.0;
                    }
                    if (neg)
                    {
                        val = -val;
                    }
                    this.Text = FormatNumber(val);
                    break;
            }

            base.OnTextChanged(e);
        }
        protected override void OnKeyPress(KeyPressEventArgs e)
        {
            bool handled = false;
            switch (e.KeyChar)
            {
                case '-':
                    if (this.Text.Length == 0)
                    {
                        break;
                    }
                    else if (this.SelectionStart == 0)
                    {
                        //negative being inserted first
                        break;
                    }
                    else
                    {
                        handled = true;
                        break;
                    }
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                case '0':
                case (char)Keys.Back:
                    break;
                case 'M':
                case 'm':
                case 'T':
                case 't':
                case '%':
                    //check last pos
                    int l = this.Text.Length;
                    int sT = this.SelectionStart;
                    int sL = this.SelectionLength;
                    if ((sT + sL) != l)
                    {
                        handled = true;
                    }
                    break;
                default:
                    string thisChar = e.KeyChar.ToString();
                    if (thisChar == decSep)
                    {
                        char[] txt = this.Text.ToCharArray();
                        for (int i = 0; i < txt.Length; i++)
                        {
                            if (txt[i].ToString() == decSep)
                            {
                                handled = true;
                                break;
                            }
                        }
                        break;
                    }
                    else if (thisChar != thouSep)
                    {
                        handled = true;
                    }
                    break;
            }

            if (!handled)
            {
                base.OnKeyPress(e);
            }
            else
            {
                e.Handled = true;
            }

        }
        protected override void OnLeave(EventArgs e)
        {
            string tmp = this.Text;
            if (tmp == "")
            {
                tmp = "0";
                numVal = NumberLostFocus(ref tmp);
                this.Text = tmp;
            }
            if (tmp.Substring(tmp.Length - 1) == "%")
            {
                tmp = tmp.Substring(0, tmp.Length - 1);
                numVal = 0.0;
                numVal = NumberLostFocus(ref tmp) / 100.0;
                double test = numVal * 100.0;
                this.Text = FormatNumber(test) + "%";
            }
            else if (perCent)
            {
                numVal = NumberLostFocus(ref tmp);
                double test = numVal * 100.0;
                this.Text = FormatNumber(test) + "%";
            }
            else
            {
                numVal = NumberLostFocus(ref tmp);
                this.Text = tmp;
            }
            base.OnLeave(e);
        }
        private string FormatNumber(double amount)
        {
            NumberFormatInfo nF = new NumberFormatInfo();
            nF.NumberDecimalSeparator = decSep;
            nF.NumberGroupSeparator = thouSep;

            string decFormat;
            if (useThouSep)
            {
                decFormat = "#,##0";
            }
            else
            {
                decFormat = "#0";
            }
            if (decPlaces > 0)
            {
                decFormat += ".";
                for (int i = 0; i < decPlaces; i++)
                {
                    decFormat += "0";
                }
                if (extraDecPlaces > 0)
                {
                    for (int i = 0; i < extraDecPlaces; i++)
                    {
                        decFormat += "#";
                    }
                }
            }
            else if (extraDecPlaces > 0)
            {
                decFormat += ".";
                for (int i = 0; i < extraDecPlaces; i++)
                {
                    decFormat += "#";
                }
            }
            return (amount.ToString(decFormat, nF));
        }
        private double NumberLostFocus(ref string amountBox)
        {
            if (amountBox.Substring(0, 1) == decSep)
                amountBox = "0" + amountBox;
            NumberFormatInfo nF = new NumberFormatInfo();
            nF.NumberDecimalSeparator = decSep;
            nF.NumberGroupSeparator = thouSep;

            try
            {
                double d = 0.0;
                int l = amountBox.Length;
                if (l > 0)
                {

                    char[] c = amountBox.ToCharArray();
                    char endChar = c[l - 1];

                    switch (endChar)
                    {
                        case '0':
                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                        case '8':
                        case '9':
                            {
                                stripNonNumerics(ref amountBox);
                                d = Double.Parse(amountBox, nF);
                                break;
                            }
                        case 'm':
                        case 'M':
                            {
                                if (amountBox.Length == 1)
                                    d = 1000000.0;
                                else
                                {
                                    string s = amountBox.Substring(0, l - 1);
                                    stripNonNumerics(ref s);
                                    d = Double.Parse(s, nF) * 1000000.0;
                                }
                                break;

                            }
                        case 't':
                        case 'T':
                            {
                                if (amountBox.Length == 1)
                                    d = 1000.0;
                                else
                                {
                                    string s = amountBox.Substring(0, l - 1);
                                    stripNonNumerics(ref s);
                                    d = Double.Parse(s, nF) * 1000.0;
                                }
                                break;
                            }
                        default:
                            {
                                //remove offending char
                                string s = amountBox.Substring(0, l - 1);
                                if (s.Length > 0)
                                {
                                    stripNonNumerics(ref s);
                                    d = Double.Parse(s, nF);
                                }
                                else
                                    d = 0.0;
                                break;
                            }
                    }
                }
                amountBox = FormatNumber(d);
                return (d);
            }
            catch (Exception e)
            {
                //handle exception here;
                return 0.0;
            }
        }
        private void stripNonNumerics(ref string amountBox)
        {
            bool dSFound = false;
            char[] tmp = decSep.ToCharArray();
            char dS = tmp[0];
            string cleanNum = "";
            int l = amountBox.Length;
            if (l > 0)
            {
                char[] c = amountBox.ToCharArray();
                for (int i = 0; i < l; i++)
                {
                    char b = c[i];
                    switch (b)
                    {
                        case '0':
                        case '1':
                        case '2':
                        case '3':
                        case '4':
                        case '5':
                        case '6':
                        case '7':
                        case '8':
                        case '9':
                            cleanNum += b;
                            break;
                        case '-':
                            if (i == 0)
                                cleanNum += b;
                            break;
                        default:
                            if ((b == dS) && (!dSFound))
                            {
                                dSFound = true;
                                cleanNum += b;
                            }
                            break;
                    }
                }
            }
            amountBox = cleanNum;
        }
    }

As well as restricting input to numbers, this class has a few special features.  It exposes a property Value to represent the double value of the number, it formats the text, optionally with thousand separators, and it provides short-hand entry of large numbers: 10M expands on leave to 10,000,000.00 (the number of decimal places being a property).  For the sake of brevity, the decimal and thousand separators have been hard-coded.  In a production system, these are also user preferences.


