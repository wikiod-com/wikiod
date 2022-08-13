---
title: "Supporting Video Streaming and Pixel Array Assignment to an Image Control"
slug: "supporting-video-streaming-and-pixel-array-assignment-to-an-image-control"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Parameters
| Parameters | Details |
| ------ | ------ |
| PixelHeight (System.Int32) | The height of the image in units of image pixels| 
| PixelWidth (System.Int32)  | The width of the image in units of image pixels| 
| PixelFormat (System.Windows.Media.PixelFormat)  | The width of the image in units of image pixels|
|Pixels| Anything which implements IList&lt;T&gt;- including the C# byte array|
|DpiX| Specifies the horizontal Dpi - Optional|
|DpiY| Specifies the vertical Dpi - Optional|


<ul>
<li>
Make sure to reference the <i>System.Windows.Interactivity assembly</i>, so that the XAML Parser will recognize the <i>xmlns:i</i> declaration.
</li>
<li>
Note that the <i>xmlns:b</i> statement matches the namespace where the behavior implementation resides
</li>
<li>
Example assumes working knowledge of binding expressions and XAML.
</li>
<li>This behavior supports assigning pixels to an image in the form of a byte array - even though the Dependency Property type is specified as an <i>IList<byte></i>. This works since the C# byte array implements the <i>IList<byte></i></li> interface.
<li>Behavior achieves very high performance, and can be used for Video Streaming</li>
<li>Do not assign the image's <i>Source</i> Dependency Property- bind to the <i>Pixels</i> Dependency Property instead</li>
<li>The Pixels, PixelWidth, PixelHeight and PixelFormat properties must be assigned for the pixels to be rendered</li>
<li>Order of Dependency Property assignment does not matter</li>
</ul>

## Behavior Implementation
    using System;
    using System.Collections.Generic;
    using System.Runtime.InteropServices;
    using System.Threading.Tasks;
    using System.Windows;
    using System.Windows.Controls;
    using System.Windows.Interactivity;
    using System.Windows.Media;
    using System.Windows.Media.Imaging;

    namespace MyBehaviorAssembly
    {
        public class PixelSupportBehavior : Behavior<Image>
        {

        public static readonly DependencyProperty PixelsProperty = DependencyProperty.Register(
            "Pixels", typeof (IList<byte>), typeof (PixelSupportBehavior), new PropertyMetadata(default(IList<byte>),OnPixelsChanged));

        private static void OnPixelsChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior) d;
            var pixels = (IList<byte>) e.NewValue;

            b.RenderPixels(pixels);
        }
     
        public IList<byte> Pixels
        {
            get { return (IList<byte>) GetValue(PixelsProperty); }
            set { SetValue(PixelsProperty, value); }
        }

        public static readonly DependencyProperty PixelFormatProperty = DependencyProperty.Register(
            "PixelFormat", typeof (PixelFormat), typeof (PixelSupportBehavior), new PropertyMetadata(PixelFormats.Default,OnPixelFormatChanged));

       
        private static void OnPixelFormatChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior) d;
            var pixelFormat = (PixelFormat) e.NewValue;

            if(pixelFormat==PixelFormats.Default)
                return;

            b._pixelFormat = pixelFormat;

            b.InitializeBufferIfAttached();
        }

        public PixelFormat PixelFormat
        {
            get { return (PixelFormat) GetValue(PixelFormatProperty); }
            set { SetValue(PixelFormatProperty, value); }
        }

        public static readonly DependencyProperty PixelWidthProperty = DependencyProperty.Register(
            "PixelWidth", typeof (int), typeof (PixelSupportBehavior), new PropertyMetadata(default(int),OnPixelWidthChanged));

        private static void OnPixelWidthChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior)d;
            var value = (int)e.NewValue;

            if(value<=0)
                return;

            b._pixelWidth = value;

            b.InitializeBufferIfAttached();
        }

        public int PixelWidth
        {
            get { return (int) GetValue(PixelWidthProperty); }
            set { SetValue(PixelWidthProperty, value); }
        }


        public static readonly DependencyProperty PixelHeightProperty = DependencyProperty.Register(
            "PixelHeight", typeof (int), typeof (PixelSupportBehavior), new PropertyMetadata(default(int),OnPixelHeightChanged));

        private static void OnPixelHeightChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior)d;
            var value = (int)e.NewValue;

            if (value <= 0)
                return;

            b._pixelHeight = value;

            b.InitializeBufferIfAttached();
        }

        public int PixelHeight
        {
            get { return (int) GetValue(PixelHeightProperty); }
            set { SetValue(PixelHeightProperty, value); }
        }

        public static readonly DependencyProperty DpiXProperty = DependencyProperty.Register(
            "DpiX", typeof (int), typeof (PixelSupportBehavior), new PropertyMetadata(96,OnDpiXChanged));

        private static void OnDpiXChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior)d;
            var value = (int)e.NewValue;

            if (value <= 0)
                return;

            b._dpiX = value;

            b.InitializeBufferIfAttached();
        }

        public int DpiX
        {
            get { return (int) GetValue(DpiXProperty); }
            set { SetValue(DpiXProperty, value); }
        }

        public static readonly DependencyProperty DpiYProperty = DependencyProperty.Register(
            "DpiY", typeof (int), typeof (PixelSupportBehavior), new PropertyMetadata(96,OnDpiYChanged));

        private static void OnDpiYChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            var b = (PixelSupportBehavior)d;
            var value = (int)e.NewValue;

            if (value <= 0)
                return;

            b._dpiY = value;

            b.InitializeBufferIfAttached();
        }

        public int DpiY
        {
            get { return (int) GetValue(DpiYProperty); }
            set { SetValue(DpiYProperty, value); }
        }

        private IntPtr _backBuffer = IntPtr.Zero;
        private int _bytesPerPixel;
        private const int BitsPerByte = 8;
        private int _pixelWidth;
        private int _pixelHeight;
        private int _dpiX;
        private int _dpiY;
        private Int32Rect _imageRectangle;
        private readonly GCHandle _defaultGcHandle = new GCHandle();
        private PixelFormat _pixelFormat;

        private int _byteArraySize;
        private WriteableBitmap _bitMap;

        private bool _attached;

        protected override void OnAttached()
        {
            _attached = true;
            InitializeBufferIfAttached();
        }

        private void InitializeBufferIfAttached()
        {
            if(_attached==false)
                return;

            ReevaluateBitsPerPixel();
            
            RecomputeByteArraySize();

            ReinitializeImageSource();
        }

        private void ReevaluateBitsPerPixel()
        {
            var f = _pixelFormat;

            if (f == PixelFormats.Default)
            {
                _bytesPerPixel = 0;
                return;
            };

            _bytesPerPixel = f.BitsPerPixel/BitsPerByte;
        }

        private void ReinitializeImageSource()
        {
            var f = _pixelFormat;
            var h = _pixelHeight;
            var w = _pixelWidth;

            if (w<=0|| h<=0 || f== PixelFormats.Default)
                return;

            _imageRectangle = new Int32Rect(0, 0, w, h);
            _bitMap = new WriteableBitmap(w, h, _dpiX, _dpiY, f, null);
            _backBuffer = _bitMap.BackBuffer;
            AssociatedObject.Source = _bitMap;
        }

        private void RenderPixels(IList<byte> pixels)
        {
            if (pixels == null)
            {
                return;
            }

            var buffer = _backBuffer;
            if (buffer == IntPtr.Zero)
                return;

            var size = _byteArraySize;

            var gcHandle = _defaultGcHandle;
            var allocated = false;
            var bitMap = _bitMap;
            var rect = _imageRectangle;
            var w = _pixelWidth;
            var locked = false;
            try
            {
                gcHandle = GCHandle.Alloc(pixels, GCHandleType.Pinned);
                allocated = true;

                bitMap.Lock();
                locked = true;
                var ptr = gcHandle.AddrOfPinnedObject();
                _bitMap.WritePixels(rect, ptr, size,w);                            
            }
            finally
            {
                if(locked)
                    bitMap.Unlock();

                if (allocated)
                    gcHandle.Free();
            }
        }

        private void RecomputeByteArraySize()
        {
            var h = _pixelHeight;
            var w = _pixelWidth;
            var bpp = _bytesPerPixel;

            if (w<=0|| h<=0 || bpp<=0)
                return;

            _byteArraySize = (w * h * bpp);
        }

        public PixelSupportBehavior()
        {
            _pixelFormat = PixelFormats.Default;
        }
    }
    }


## XAML Usage

    <UserControl x:Class="Example.View"
             xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
             xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
             xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" 
             xmlns:d="http://schemas.microsoft.com/expression/blend/2008"
             xmlns:b="clr-namespace:MyBehaviorAssembly;assembly=MyBehaviorAssembly"
             xmlns:i="http://schemas.microsoft.com/expression/2010/interactivity"
             mc:Ignorable="d" 
             d:DesignHeight="200" d:DesignWidth="200"                    
             >

                    <Image Stretch="Uniform">             

                    <i:Interaction.Behaviors>

                        <b:PixelSupportBehavior 
                                PixelHeight="{Binding PixelHeight}"
                                PixelWidth="{Binding PixelWidth}"
                                PixelFormat="{Binding PixelFormat}"
                                Pixels="{Binding Pixels}"
                                />

                    </i:Interaction.Behaviors>

                </Image>

    </UserControl>



