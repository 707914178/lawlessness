package cn.lawlessness.sqlprovider.model;

import cn.lawlessness.sqlprovider.enums.OrderByType;

/**
 * @author liutao
 * @date 2023/9/8 16:25
 */
public class PageOrderBy {


    private String fieldId;

    private String type;

    public PageOrderBy() {}

    public PageOrderBy(String fieldId) {
        this.fieldId = fieldId;
    }

    public PageOrderBy(String fieldId, String type) {
        this.fieldId = fieldId;
        this.type = type;
    }
    
    public static PageOrderBy desc(String fieldId) {
        return new PageOrderBy(fieldId, OrderByType.DESC.getType());
    }

    public static PageOrderBy asc(String fieldId) {
        return new PageOrderBy(fieldId, OrderByType.ASC.getType());
    }

    public String getFieldId() {
        return fieldId;
    }

    public void setFieldId(String fieldId) {
        this.fieldId = fieldId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
