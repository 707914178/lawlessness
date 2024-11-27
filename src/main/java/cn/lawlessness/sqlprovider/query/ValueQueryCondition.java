package cn.lawlessness.sqlprovider.query;

import java.io.Serializable;

/**
 * @author liutao
 * @date 2023/9/8 16:39
 */
public class ValueQueryCondition extends QueryCondition implements Serializable {

    private static final long serialVersionUID = -7843953992095558961L;

    @Override
    public QueryCondition copy() {
        QueryCondition queryCondition = new ValueQueryCondition();
        queryCondition.fieldId = this.fieldId;
        queryCondition.op = this.op;
        queryCondition.value = this.value;
        return queryCondition;
    }
}
