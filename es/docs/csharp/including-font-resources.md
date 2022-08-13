---
title: "Incluir recursos de fuentes"
slug: "incluir-recursos-de-fuentes"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parámetros
| Parámetro| Detalles|
| ------ | ------ |
| fontbytes|matriz de bytes del binario .ttf

## Crea una instancia de 'Fontfamily' desde Recursos

    public FontFamily Maneteke = GetResourceFontFamily(Properties.Resources.manteka);


[![ingrese la descripción de la imagen aquí][1]][1]


[1]: https://i.stack.imgur.com/1fneu.png

## Método de integración
  
    public static FontFamily GetResourceFontFamily(byte[] fontbytes)
    {
        PrivateFontCollection pfc = new PrivateFontCollection();
        IntPtr fontMemPointer = Marshal.AllocCoTaskMem(fontbytes.Length);
        Marshal.Copy(fontbytes, 0, fontMemPointer, fontbytes.Length);
        pfc.AddMemoryFont(fontMemPointer, fontbytes.Length);
        Marshal.FreeCoTaskMem(fontMemPointer);
        return pfc.Families[0];
    }

## Uso con un 'Botón'
        public static class Res
        {
            /// <summary>
            /// URL: https://www.behance.net/gallery/2846011/Manteka
            /// </summary>
            public static FontFamily Maneteke = GetResourceFontFamily(Properties.Resources.manteka);

            public static FontFamily GetResourceFontFamily(byte[] fontbytes)
            {
                PrivateFontCollection pfc = new PrivateFontCollection();
                IntPtr fontMemPointer = Marshal.AllocCoTaskMem(fontbytes.Length);
                Marshal.Copy(fontbytes, 0, fontMemPointer, fontbytes.Length);
                pfc.AddMemoryFont(fontMemPointer, fontbytes.Length);
                Marshal.FreeCoTaskMem(fontMemPointer);
                return pfc.Families[0];
            }
        }
    
        public class FlatButton : Button
        {
            public FlatButton() : base()
            {
                Font = new Font(Res.Maneteke, Font.Size);
            }
    
            protected override void OnFontChanged(EventArgs e)
            {
                base.OnFontChanged(e);
                this.Font = new Font(Res.Maneteke, this.Font.Size);
            }
        }

