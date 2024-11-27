package cn.lawlessness.sqlprovider.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author liutao
 * @date 2024/11/27
 */
public class QueryFilter implements Serializable, Cloneable {

    private static final long serialVersionUID = 5705624504587039658L;
    /**
     * 操作符
     */
    private String op;

    /**
     * 子条件
     */
    private List<QueryCondition> children;


    public QueryFilter clone() {
        try {
            super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException(e);
        }
        QueryFilter queryFilter = new QueryFilter();
        queryFilter.op = this.op;
        if (null != children) {
            queryFilter.children = new ArrayList<>();
            for (QueryCondition child : this.children) {
                queryFilter.children.add(child.clone());
            }
        }
        return queryFilter;
    }

    public void addCondition(QueryCondition queryCondition) {
        if (null == children) {
            children = new ArrayList<>();
        }
        children.add(queryCondition);
    }

    public List<QueryCondition> getChildren() {
        return children;
    }

    public String getOp() {
        return op;
    }

    public void setChildren(List<QueryCondition> children) {
        this.children = children;
    }

    public void setOp(String op) {
        this.op = op;
    }
}
