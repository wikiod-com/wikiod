---
title: "Custom ListView"
slug: "custom-listview"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Custom Listview comprises of rows that are designed as per the users needs.
[![enter image description here][1]][1]


For the layout above your customrow.axml file is as shown below

        <?xml version="1.0" encoding="utf-8"?>
    <RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
        android:layout_width="fill_parent"
        android:layout_height="wrap_content"
        android:padding="8dp">
        <ImageView
            android:id="@+id/Image"
            android:layout_width="80dp"
            android:layout_height="80dp"
            android:layout_alignParentLeft="true"
            android:layout_marginRight="8dp"
            android:src="@drawable/icon" />
        <TextView
            android:id="@+id/Text1"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_alignTop="@id/Image"
            android:layout_toRightOf="@id/Image"
            android:layout_marginTop="5dip"
            android:text="This is Line1"
            android:textSize="20dip"
            android:textStyle="bold" />
        <TextView
            android:id="@+id/Text2"
            android:layout_width="fill_parent"
            android:layout_height="wrap_content"
            android:layout_below="@id/Text1"
            android:layout_marginTop="1dip"
            android:text="This is line2"
            android:textSize="15dip"
            android:layout_toRightOf="@id/Image" />
    </RelativeLayout> 

Then you can design your main.axml, which contains a textview for the header and a listview.
 
[![enter image description here][2]][2]

Hope that is easy...

Next create your Data.cs class that will represent your row objects 

    public class Data
    {
        public string Heading;
        public string SubHeading;
        public string ImageURI;

        public Data ()
        {
            Heading = "";
            SubHeading = "";
            ImageURI = "";
        }
    }

Next you need the DataAdapter.cs class, Adapters link your data with the underlying view 

    public class DataAdapter : BaseAdapter<Data> {

        List<Data> items;

        Activity context;
        public DataAdapter(Activity context, List<Data> items)
            : base()
        {
            this.context = context;
            this.items = items;
        }
        public override long GetItemId(int position)
        {
            return position;
        }
        public override Data this[int position]
        {
            get { return items[position]; }
        }
        public override int Count
        {
            get { return items.Count; }
        }
        public override View GetView(int position, View convertView, ViewGroup parent)
        {
            var item = items[position];
            View view = convertView;
            if (view == null) // no view to re-use, create new
                view = context.LayoutInflater.Inflate(Resource.Layout.CustomRow, null);

            view.FindViewById<TextView>(Resource.Id.Text1).Text = item.Heading;
            view.FindViewById<TextView>(Resource.Id.Text2).Text = item.SubHeading;

            var imageBitmap = GetImageBitmapFromUrl(item.ImageURI);
            view.FindViewById<ImageView> (Resource.Id.Image).SetImageBitmap (imageBitmap);
            return view;
        }

        private Bitmap GetImageBitmapFromUrl(string url)
        {
            Bitmap imageBitmap = null;
            if(!(url=="null"))
                using (var webClient = new WebClient())
                {
                    var imageBytes = webClient.DownloadData(url);
                    if (imageBytes != null && imageBytes.Length > 0)
                    {
                        imageBitmap = BitmapFactory.DecodeByteArray(imageBytes, 0, imageBytes.Length);
                    }
                }

            return imageBitmap;
        }

    } 

The most important part is inside the GetView Function, this is where you link your object to your custom row.

[![enter image description here][3]][3]


The GetImageBitmapFromUrl is not part of the dataadapter but I have put this over here for simplicity.

At last we come to the MainActivity.cs

    public class MainActivity : Activity
    {
            
        ListView listView;
            
        protected override void OnCreate (Bundle bundle)
        {
            base.OnCreate (bundle);

            // Set our view from the "main" layout resource
            SetContentView (Resource.Layout.Main);
            listView = FindViewById<ListView>(Resource.Id.List); 

            List<Data> myList = new List<Data> ();

            Data obj = new Data ();
            obj.Heading = "Apple";
            obj.SubHeading = "An Apple a day keeps the doctor away";
            obj.ImageURI = "http://www.thestar.com/content/dam/thestar/opinion/editorials/star_s_view_/2011/10/12/an_apple_a_day_not_such_a_good_idea/apple.jpeg";

            myList.Add (obj);

            Data obj1 = new Data();
            obj1.Heading = "Banana";
            obj1.SubHeading = "Bananas are an excellent source of vitamin B6 ";
            obj1.ImageURI = "http://www.bbcgoodfood.com/sites/bbcgoodfood.com/files/glossary/banana-crop.jpg";

            myList.Add(obj1);

            Data obj2 = new Data();
            obj2.Heading = "Kiwi Fruit";
            obj2.SubHeading = "Kiwifruit is a rich source of vitamin C";
            obj2.ImageURI = "http://www.wiffens.com/wp-content/uploads/kiwi.png";

            myList.Add(obj2);

            Data obj3 = new Data();
            obj3.Heading = "Pineapple";
            obj3.SubHeading = "Raw pineapple is an excellent source of manganese";
            obj3.ImageURI = "http://www.medicalnewstoday.com/images/articles/276/276903/pineapple.jpg";

            myList.Add(obj3);

            Data obj4 = new Data();
            obj4.Heading = "Strawberries";
            obj4.SubHeading = "One serving (100 g)of strawberries contains approximately 33 kilocalories";
            obj4.ImageURI = "https://ecs3.tokopedia.net/newimg/product-1/2014/8/18/5088/5088_8dac78de-2694-11e4-8c99-6be54908a8c2.jpg";

            myList.Add (obj4);
            listView.Adapter = new DataAdapter(this,myList);

        }

Your final project structure is as show below.

[![enter image description here][4]][4]

If everything is fine you should see the output as shown 

[![enter image description here][5]][5] 


  [1]: http://i.stack.imgur.com/pDN3Z.png
  [2]: http://i.stack.imgur.com/c13PZ.png
  [3]: http://i.stack.imgur.com/dne0A.png
  [4]: http://i.stack.imgur.com/jf7Pa.png
  [5]: http://i.stack.imgur.com/Bsvpp.png

