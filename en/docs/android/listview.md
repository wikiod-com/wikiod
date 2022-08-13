---
title: "ListView"
slug: "listview"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

ListView is a viewgroup which groups several items from a data source like array or database and displays them in a scroll-able list. Data are bound with listview using an Adapter class.

[`ListView`][1] is a view group that displays a list of scrollable items.   
The list items are automatically inserted to the list using an [`Adapter`][2] that pulls content from a source such as an array or database query and converts each item result into a view that's placed into the list.


When the content for your layout is dynamic or not pre-determined, you can use a layout that subclasses [`AdapterView`][3] to populate the layout with views at runtime. A subclass of the `AdapterView` class uses an [`Adapter`][2] to bind data to its layout. 

Before using the `ListView` you should also checking the [`RecyclerView`][4] examples.


  [1]: https://developer.android.com/reference/android/widget/ListView.html
  [2]: https://developer.android.com/reference/android/widget/Adapter.html
  [3]: https://developer.android.com/reference/android/widget/AdapterView.html
  [4]: https://www.wikiod.com/android/recyclerview

## Custom ArrayAdapter
[By default][1] the ArrayAdapter class creates a view for each array item by calling `toString()` on each item and placing the contents in a TextView.

To create a complex view for each item (for example, if you want an ImageView for each array item), extend the ArrayAdapter class and override the `getView()` method to return the type of View you want for each item.

For example:

    public class MyAdapter extends ArrayAdapter<YourClassData>{

        private LayoutInflater inflater;

        public MyAdapter (Context context, List<YourClassData> data){
            super(context, 0, data);
            inflater = LayoutInflater.from(context);
        }

        @Override
        public long getItemId(int position)
        {
            //It is just an example
            YourClassData data = (YourClassData) getItem(position);
            return data.ID;
        }
    
        @Override
        public View getView(int position, View view, ViewGroup parent)
        {
            ViewHolder viewHolder;
            if (view == null) {
                view = inflater.inflate(R.layout.custom_row_layout_design, null);
                // Do some initialization
            
                //Retrieve the view on the item layout and set the value.
                viewHolder = new ViewHolder(view);
                view.setTag(viewHolder);
             }
             else {
                 viewHolder = (ViewHolder) view.getTag();
             }
             
            //Retrieve your object    
            YourClassData data = (YourClassData) getItem(position);
           
            viewHolder.txt.setTypeface(m_Font);    
            viewHolder.txt.setText(data.text);              
            viewHolder.img.setImageBitmap(BitmapFactory.decodeFile(data.imageAddr));
            
            return view;
        
        }

        private class ViewHolder
        {
             private final TextView txt;
             private final ImageView img;

             private ViewHolder(View view) 
             {
                 txt = (TextView) view.findViewById(R.id.txt);
                 img = (ImageView) view.findViewById(R.id.img);
             }
        }
    }


  [1]: https://www.wikiod.com/android/listview#A basic ListView with an ArrayAdapter

## A basic ListView with an ArrayAdapter
By default the [`ArrayAdapter`][1] creates a view for each array item by calling `toString()` on each item and placing the contents in a `TextView`.

Example:

    ArrayAdapter<String> adapter = new ArrayAdapter<String>(this,
            android.R.layout.simple_list_item_1, myStringArray);

where `android.R.layout.simple_list_item_1` is the layout that contains a `TextView` for each string in the array.

Then simply call `setAdapter()` on your `ListView`:

    ListView listView = (ListView) findViewById(R.id.listview);
    listView.setAdapter(adapter);

To use something other than TextViews for the array display, for instance, ImageViews, or to have some of data besides `toString()` results fill the views, override `getView(int, View, ViewGroup)` to return the type of view you want. [Check this example][2].


  [1]: https://developer.android.com/reference/android/widget/ArrayAdapter.html
  [2]: https://www.wikiod.com/android/listview#Custom ArrayAdapter

## Filtering with CursorAdapter


