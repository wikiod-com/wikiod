---
title: "Incluindo recursos de fonte"
slug: "incluindo-recursos-de-fonte"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Parâmetros
| Parâmetro| Detalhes|
| ------ | ------ |
| array fontbytes|byte do binário .ttf

## Instanciar 'Fontfamily' de Recursos

    public FontFamily Maneteke = GetResourceFontFamily(Properties.Resources.manteka);


[![digite a descrição da imagem aqui][1]][1]


[1]: https://i.stack.imgur.com/1fneu.png

## Método de integração
  
    public static FontFamily GetResourceFontFamily(byte[] fontbytes)
    {
        PrivateFontCollection pfc = new PrivateFontCollection();
        IntPtr fontMemPointer = Marshal.AllocCoTaskMem(fontbytes.Length);
        Marshal.Copy(fontbytes, 0, fontMemPointer, fontbytes.Length);
        pfc.AddMemoryFont(fontMemPointer, fontbytes.Length);
        Marshal.FreeCoTaskMem(fontMemPointer);
        return pfc.Families[0];
    }

## Uso com um 'Botão'
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

