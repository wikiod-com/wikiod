---
title: "Query Structure"
slug: "query-structure"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Selecting Data
The following functions allow you to build SQL SELECT statements.

    $this->db->get()

This runs the selection query and returns the result. Can be used by itself to retrieve all records from a table:

    $query = $this->db->get('tablename');  // Produces: SELECT * FROM tablename

The second and third parameters enable you to set a limit and offset clause:

    $query = $this->db->get('tablename', 10, 20);

    // Executes: SELECT * FROM tablename LIMIT 20, 10
    // (in MySQL. Other databases have slightly different syntax)

## Selecting Data
**Selecting data with condition**

    $query  = $this->db->select('*')
                       ->from('table_name')
                       ->where('column_name', $value) // Condition 
                       ->get();
    return $query->result();

**Selecting data with multiple conditions**

    $conditions = array('column_name_1' => $value_1, 'column_name_2' => $value_2);
    $query  = $this->db->select('*')
                       ->from('table_name')
                       ->where($conditions) // Conditions
                       ->get();
    return $query->result();

**Select data with condition and limit**

    $query  = $this->db->select('*')
                       ->from('table_name')
                       ->where('column_name', $value) // Condition
                       ->limit(10) // Maximum 10 rows
                       ->get();
    return $query->result();

**Select data with condition, maximum rows and order descending**

    $query  = $this->db->select('*')
                       ->from('table_name')
                       ->where('column_name', $value) // Condition
                       ->limit(10) // Maximum 10 rows 
                       ->order_by('id','DESC') // Order data descending 
                       ->get();
    return $query->result();

## Selecting data with second Optional Parameter
Usually we are not using second parameter in ```select([$select = '*'[, $escape = NULL]])``` in CodeIgniter.
If you set it to FALSE, CodeIgniter will not try to protect your field or table names.

In the following example, we are going to select the datetime type field by formatting it using sql query and set it ```FALSE``` (By doing this, we are going to tell CI not to escape the query automatically).

    public function getUserInfo($id)
    {
        $this->db->select('BaseTbl.id, BaseTbl.name, DATE_FORMAT(BaseTbl.createdDtm, "%d-%m-%Y") AS createdDtm', FALSE); // FALSE is the second optional parameter
        $this->db->from('tbl_users as BaseTbl');
        $this->db->where('isDeleted', 0);
        $this->db->where('BaseTbl.id', $id);
        $query = $this->db->get();
                
        return $query->result();
    }

If we are not set it to ```FALSE```, it will automatically escapes and break the query.

## Join Tables Using Query Builder
Sometimes we need to join multiple tables to get aggregate data in return. here is how we can achieve the same using CodeIgniter Query Builder / Active Records.

    public function getStudentInfo($studentid){
        $query = $this->db->select("st.id, st.name, st.class, mk.maths, mk.science")
                   ->from("students as st")
                   ->join("marks as mk", "mk.student_id = st.id", "inner")
                   ->where("st.id", $studentId)
                   ->get();
        return $query->result();        
    }

Here we use join() to join multiple tables and we can change join type in 3rd parameter like "inner", "left", "right" etc.



