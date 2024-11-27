package cn.lawlessness.sqlprovider.query;

import cn.lawlessness.sqlprovider.enums.ConditionOp;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author liutao
 * @date 2024/11/27
 */

public class QueryCondition implements Serializable,Cloneable {

    private static final long serialVersionUID = 4631365623454101504L;
    /**
     * 字段ID
     */
    protected String fieldId;

    /**
     * 操作符
     */
    protected String op;

    /**
     * 值
     */
    protected Object value;

    /**
     * 子条件
     */
    protected QueryFilter filter;

    public QueryCondition() {

    }

    public QueryCondition(String fieldId, String op, Object value) {
        this.fieldId = fieldId;
        this.op = op;
        this.value = value;
    }

    public QueryCondition(String fieldId, Object value) {
        this.fieldId = fieldId;
        this.op = ConditionOp.EQ.getCode();
        this.value = value;
    }

    public static List<QueryCondition> of(Object ... objs) {
        List<QueryCondition> list = new ArrayList<>();
        for (int i = 0; i < objs.length; i += 2) {
            QueryCondition queryCondition = new QueryCondition();
            queryCondition.setFieldId(objs[i].toString());
            queryCondition.setValue(objs[i + 1]);
            list.add(queryCondition);
        }
        return list;
    }


    public static QueryCondition isNull(String fieldId) {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = fieldId;
        queryCondition.op = ConditionOp.ISNULL.getCode();
        return queryCondition;
    }
    
    public static QueryCondition gt(String fieldId, BigDecimal val) {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = fieldId;
        queryCondition.op = ConditionOp.GT.getCode();
        queryCondition.value = val;
        return queryCondition;
    }

    public static QueryCondition ne(String fieldId, Object val) {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = fieldId;
        queryCondition.op = ConditionOp.NE.getCode();
        queryCondition.value = val;
        return queryCondition;
    }

    public static QueryCondition eNe(String fieldId, Object val) {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = fieldId;
        queryCondition.op = ConditionOp.E_NE.getCode();
        queryCondition.value = val;
        return queryCondition;
    }
    

    public static QueryCondition gtZero(String fieldId) {
        return gt(fieldId, BigDecimal.ZERO);
    }

    public static QueryCondition neZero(String fieldId) {
        return ne(fieldId, BigDecimal.ZERO);
    }

    public static QueryCondition eNeZero(String fieldId) {
        return eNe(fieldId, BigDecimal.ZERO);
    }
    
    
    public static QueryCondition notNull(String fieldId) {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = fieldId;
        queryCondition.op = ConditionOp.NOTNULL.getCode();
        return queryCondition;
    }


    public QueryCondition copy() {
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.fieldId = this.fieldId;
        queryCondition.op = this.op;
        queryCondition.value = this.value;
        return queryCondition;
    }

    @Override
    public QueryCondition clone() {
        try {
            super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
        QueryCondition queryCondition = this.copy();
        QueryFilter filter = this.getFilter();
        if (null != filter) {
            queryCondition.filter = filter.clone();
        }
        return queryCondition;
    }

    public boolean containsFieldId(String fieldId) {
        if (null != this.fieldId && this.fieldId.equals(fieldId)) {
            return true;
        }
        if (null != this.filter) {
            List<QueryCondition> children = this.filter.getChildren();
            for (QueryCondition childCondition : children) {
                if (childCondition.containsFieldId(fieldId)) {
                    return true;
                }
            }
        }
        return false;
    }

    public static QueryCondition or(QueryCondition ...conditions) {
        QueryFilter queryFilter = new QueryFilter();
        queryFilter.setOp("or");
        List<QueryCondition> filterConditionList = new ArrayList<>(Arrays.asList(conditions));
        queryFilter.setChildren(filterConditionList);
        QueryCondition queryCondition = new QueryCondition();
        queryCondition.setFilter(queryFilter);
        return queryCondition;
    }

    public void setFilter(QueryFilter filter) {
        this.filter = filter;
    }

    public QueryFilter getFilter() {
        return filter;
    }

    public String getOp() {
        return op;
    }

    public Object getValue() {
        return value;
    }

    public String getFieldId() {
        return fieldId;
    }

    public void setOp(String op) {
        this.op = op;
    }

    public void setFieldId(String fieldId) {
        this.fieldId = fieldId;
    }

    public void setValue(Object value) {
        this.value = value;
    }
}
