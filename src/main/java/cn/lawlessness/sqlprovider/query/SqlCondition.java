package cn.lawlessness.sqlprovider.query;

import java.io.Serializable;

/**
 * @author liutao
 * @date 2024/12/10 16:05
 */
public class SqlCondition extends QueryCondition implements Serializable {
    private static final long serialVersionUID = -8154945782050867048L;
    
    private String sql;
    
    public SqlCondition(String sql) {
        this.sql = sql;
    }
    
    public String getSql () {
        return this.sql;
    }
    
}
