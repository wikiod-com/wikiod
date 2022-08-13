---
title: "How to insert data into table using DynamoDb?"
slug: "how-to-insert-data-into-table-using-dynamodb"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Import a CSV file into a DynamoDB table using boto (Python package)
The Python function `import_csv_to_dynamodb(table_name, csv_file_name, colunm_names, column_types)` below imports a CSV file into a DynamoDB table. Column names and column must be specified. It uses [boto](http://docs.pythonboto.org/en/latest/). Below is the function as well as a demo (`main()`) and the CSV file used. 

    import boto
    
    MY_ACCESS_KEY_ID = 'copy your access key ID here'
    MY_SECRET_ACCESS_KEY = 'copy your secrete access key here'
    
    
    def do_batch_write(items, table_name, dynamodb_table, dynamodb_conn):
        '''
        From https://gist.github.com/griggheo/2698152#file-gistfile1-py-L31
        '''
        batch_list = dynamodb_conn.new_batch_write_list()
        batch_list.add_batch(dynamodb_table, puts=items)
        while True:
            response = dynamodb_conn.batch_write_item(batch_list)
            unprocessed = response.get('UnprocessedItems', None)
            if not unprocessed:
                break
            batch_list = dynamodb_conn.new_batch_write_list()
            unprocessed_list = unprocessed[table_name]
            items = []
            for u in unprocessed_list:
                item_attr = u['PutRequest']['Item']
                item = dynamodb_table.new_item(
                        attrs=item_attr
                )
                items.append(item)
            batch_list.add_batch(dynamodb_table, puts=items)
    
    
    def import_csv_to_dynamodb(table_name, csv_file_name, colunm_names, column_types):
        '''
        Import a CSV file to a DynamoDB table
        '''        
        dynamodb_conn = boto.connect_dynamodb(aws_access_key_id=MY_ACCESS_KEY_ID, aws_secret_access_key=MY_SECRET_ACCESS_KEY)
        dynamodb_table = dynamodb_conn.get_table(table_name)     
        BATCH_COUNT = 2 # 25 is the maximum batch size for Amazon DynamoDB
        
        items = []
        
        count = 0
        csv_file = open(csv_file_name, 'r')
        for cur_line in csv_file:
            count += 1
            cur_line = cur_line.strip().split(',')
            
            row = {}
            for colunm_number, colunm_name in enumerate(colunm_names):
                row[colunm_name] = column_types[colunm_number](cur_line[colunm_number])
             
            item = dynamodb_table.new_item(
                        attrs=row
                )           
            items.append(item)
            
            if count % BATCH_COUNT == 0:
                print 'batch write start ... ', 
                do_batch_write(items, table_name, dynamodb_table, dynamodb_conn)
                items = []
                print 'batch done! (row number: ' + str(count) + ')'
        
        # flush remaining items, if any
        if len(items) > 0: 
            do_batch_write(items, table_name, dynamodb_table, dynamodb_conn)

            
        csv_file.close() 
    
    
    def main():
        '''
        Demonstration of the use of import_csv_to_dynamodb()
        We assume the existence of a table named `test_persons`, with
        - Last_name as primary hash key (type: string)
        - First_name as primary range key (type: string)
        '''
        colunm_names = 'Last_name First_name'.split()
        table_name = 'test_persons'
        csv_file_name = 'test.csv'
        column_types = [str, str]
        import_csv_to_dynamodb(table_name, csv_file_name, colunm_names, column_types)
        
    
    if __name__ == "__main__":
        main()
        #cProfile.run('main()') # if you want to do some profiling



`test.csv`'s content (must be located in the same folder as the Python script):

    John,Doe
    Bob,Smith
    Alice,Lee
    Foo,Bar
    a,b
    c,d
    e,f
    g,h
    i,j
    j,l

