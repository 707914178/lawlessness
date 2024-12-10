package cn.lawlessness.sqlprovider;

import cn.lawlessness.sqlprovider.annotation.Condition;
import cn.lawlessness.sqlprovider.annotation.OrderBy;
import cn.lawlessness.sqlprovider.annotation.PageIndex;
import cn.lawlessness.sqlprovider.annotation.PageSize;
import cn.lawlessness.sqlprovider.annotation.QueryField;
import cn.lawlessness.sqlprovider.annotation.Tab;
import cn.lawlessness.sqlprovider.annotation.TabCol;
import cn.lawlessness.sqlprovider.annotation.TabJoin;
import cn.lawlessness.sqlprovider.annotation.TabJoinCol;
import cn.lawlessness.sqlprovider.annotation.TabQuery;
import cn.lawlessness.sqlprovider.core.TabProperties;
import cn.lawlessness.sqlprovider.core.TabProperties.TabJoinColProperties;
import cn.lawlessness.sqlprovider.core.TabProperties.TabJoinProperties;
import cn.lawlessness.sqlprovider.el.ElContextExecutor;
import cn.lawlessness.sqlprovider.enums.ConditionOp;
import cn.lawlessness.sqlprovider.enums.OrderByType;
import cn.lawlessness.sqlprovider.excpetion.SqlProviderException;
import cn.lawlessness.sqlprovider.model.PageLimit;
import cn.lawlessness.sqlprovider.model.PageOrderBy;
import cn.lawlessness.sqlprovider.query.QueryCondition;
import cn.lawlessness.sqlprovider.query.QueryFilter;
import cn.lawlessness.sqlprovider.query.SqlCondition;
import cn.lawlessness.sqlprovider.query.ValueQueryCondition;
import cn.lawlessness.sqlprovider.utils.ReflectUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * @copy to https://gitee.com/beibanqiu/amy-springboot.git
 * @author liutao
 * @date 2024/11/27
 */
public class SqlProviderBuilder {

    public SessionContainer sessionContainer = new SessionContainer();
    
    private String symbol = "\"";


    private static Object getFieldValue(Field field, Object object) {
        try {
            String name = field.getName();
            String methodName = new StringBuilder("get")
                    .append(String.valueOf(name.charAt(0)).toUpperCase())
                    .append(name.substring(1))
                    .toString();
            Method method = object.getClass().getMethod(methodName);
            Object invoke = method.invoke(object);
            return invoke;
        } catch (Exception ignored) {
        }
        Object val = null;
        field.setAccessible(true);
        try {
            val = field.get(object);
        } catch (IllegalAccessException ignored) {
        }
        field.setAccessible(false);
        return val;
    }
    

    private static List<Field> getAllFieldsList(final Class<?> cls) {
        Class<?> currentClass = cls;
        List<Class<?>> allCls = new ArrayList<>();
        List<List<Field>> fieldsList = new ArrayList<>();
        Set<String> fieldNames = new HashSet<>();
        while (null != currentClass) {
            Field[] declaredFields = currentClass.getDeclaredFields();
            List<Field> fields = new ArrayList<>();
            fieldsList.add(fields);
            for (int i = 0; i < declaredFields.length; i++) {
                Field declaredField = declaredFields[i];
                /**
                 * Prevent overwriting fields
                 */
                if (fieldNames.add(declaredField.getName())) {
                    fields.add(declaredField);
                }
            }
            allCls.add(currentClass);
            currentClass = currentClass.getSuperclass();
        }
        final List<Field> allFields = new ArrayList<>();
        for (int i = fieldsList.size()-1; i >= 0; i--) {
            List<Field> fields = fieldsList.get(i);
            allFields.addAll(fields);
        }
        return allFields;
    }

    /**
     * 驼峰命名转化为sql命名
     * @return
     */
    private static String camelCaseToSqlCase (String name) {
        char[] chars = name.toCharArray();
        StringBuilder sqlCaseBuilder = new StringBuilder();
        for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            if (0 != i && c > 64 && c < 91) {
                sqlCaseBuilder.append("_");
            }
            sqlCaseBuilder.append(c);
        }
        return sqlCaseBuilder.toString().toLowerCase();
    }

    public static class SessionContainer {

        private static String aliasPrefix = "t";
        public List<PageOrderBy> orderByList = new ArrayList<>();
//        private Map<String, TabField> fieldMap = new LinkedHashMap<>();
        
//        private Map<Class<? extends Annotation>, List<TabField>> annotationFieldMap = new LinkedHashMap<>();
        
        private ObjectInfo mapping;
        
        private ObjectInfo query;
        

        private Map<Class<? extends Annotation>, List<TabField>> queryAnnotationFieldMap = new LinkedHashMap<>();
        
        private Map<String, TabProperties> propertiesMap = new LinkedHashMap<>();

        private Map<String, TabJoinProperties> joinPropertiesMap = new LinkedHashMap<>();
        
        private String tabName;
        
        private String tabAlias;
        
        private PageLimit pageLimit;

//        private Object query;
//        
//        private Class<?> mainClz;
        

        public int aliasIdx = 0;

        public List<QueryCondition> conditionList = new ArrayList<>();
        
        public String sql = null;
        
        public List<Object> paramList = new ArrayList<>();
        
        private List<Object> endConditionParamList;
        
        public void addEndConditionParam(Object obj) {
            if (endConditionParamList == null) {
                endConditionParamList = new ArrayList<>();
            }
            endConditionParamList.add(obj);
        }
        
        private String createAlias(){
            return aliasPrefix + ( aliasIdx ++ );
        }
        
        public String propertyTabAlias(String property) {
            TabProperties tabProperties = propertiesMap.get(property);
            if (null == tabProperties) {
                return null;
            }
            return tabProperties.tabAlias;
        }
    }
    
    public static class ObjectInfo {
        
        public ObjectInfo(Object obj) {
            this.val = obj;
        }
        
        public Object val;
        public Map<Class<? extends Annotation>, List<TabField>> annotationFieldMap = new LinkedHashMap<>();
        public Map<String, TabField> fieldMap = new LinkedHashMap<>();
    }
    
    public static class TabField {
        private String fieldName;
        private Field field;
        private Map<Class<? extends Annotation>, Annotation> annotationMap;
    }
    
    public static SqlProviderBuilder parse(Object obj) {
        SqlProviderBuilder builder = new SqlProviderBuilder();
        Class<?> queryClz = obj.getClass();
        TabQuery tabQuery = queryClz.getAnnotation(TabQuery.class);
        Class<?> clz = tabQuery.clz();
        Tab tab = clz.getAnnotation(Tab.class);
        builder.sessionContainer.query = new ObjectInfo(obj);
        builder.sessionContainer.mapping = new ObjectInfo(clz);
        builder.sessionContainer.tabName = tab.value();
        builder.sessionContainer.tabAlias = builder.sessionContainer.createAlias();
        builder.parseObj();
        builder.parseQuery();
        return builder;
    }
    
    private void parseObj() {
        Class<?> clz = (Class<?>)sessionContainer.mapping.val;
        List<Field> fieldList = getAllFieldsList(clz);
        for (Field field : fieldList) {
            TabField tabField = new TabField();
            tabField.field = field;
            tabField.fieldName = field.getName();
            tabField.annotationMap = new HashMap<>();
            sessionContainer.mapping.fieldMap.put(tabField.fieldName, tabField);
            Annotation[] annotations = field.getAnnotations();
            if (null != annotations) {
                for (Annotation annotation : annotations) {
                    Class<? extends Annotation> annotationClz = annotation.annotationType();
                    tabField.annotationMap.put(annotationClz, annotation);
                    List<TabField> tableFields = sessionContainer.mapping.annotationFieldMap.get(annotationClz);
                    if (null == tableFields) {
                        tableFields = new ArrayList<>();
                        sessionContainer.mapping.annotationFieldMap.put(annotationClz, tableFields);
                    }
                    tableFields.add(tabField);
                }
            }
        }
        List<TabField> tabJoinFieldList = sessionContainer.mapping.annotationFieldMap.get(TabJoin.class);

        List<TabJoinProperties> nextTabJoinPropertiesList = new ArrayList<>();
        if (null != tabJoinFieldList) {
            for (TabField tabJoinField : tabJoinFieldList) {
                TabJoinProperties properties = new TabJoinProperties();
                TabJoin tabJoin = (TabJoin)tabJoinField.annotationMap.get(TabJoin.class);
                properties.col = tabJoin.value();
                properties.fieldId = tabJoinField.fieldName;
                properties.tabName = tabJoin.tab();
                properties.onValueList = null == tabJoin.onValues() ? new ArrayList<>() : Arrays.asList(tabJoin.onValues());
                sessionContainer.propertiesMap.put(properties.fieldId, properties);
                if (!"".equals(tabJoin.joinFieldId())) {
                    properties.joinFieldId = tabJoin.joinFieldId();
                    nextTabJoinPropertiesList.add(properties);
                } else {
                    properties.tabAlias = sessionContainer.createAlias();
                    sessionContainer.joinPropertiesMap.put(properties.fieldId, properties);
                }
            }
        }
        for (TabJoinProperties tabJoinField : nextTabJoinPropertiesList) {
            TabJoinProperties leftProperties = (TabJoinProperties)sessionContainer.propertiesMap.get(tabJoinField.joinFieldId);
            tabJoinField.tabAlias = sessionContainer.createAlias();
            leftProperties.subJoinList.add(tabJoinField);
        }
        // 关联字段
        List<TabField> tabJoinColFieldList = sessionContainer.mapping.annotationFieldMap.get(TabJoinCol.class);
        if (null != tabJoinColFieldList) {
            for (TabField tabJoinColField : tabJoinColFieldList) {
                TabJoinColProperties properties = new TabJoinColProperties();
                TabJoinCol tabJoinCol = (TabJoinCol)tabJoinColField.annotationMap.get(TabJoinCol.class);
                properties.col = tabJoinCol.col();
                properties.fieldId = tabJoinColField.fieldName;
                properties.joinFieldId = tabJoinCol.fieldId();
                TabProperties tabProperties = sessionContainer.propertiesMap.get(properties.joinFieldId);
                if (null == tabProperties) {
                    throw new SqlProviderException("unknown join field id: " + properties.joinFieldId);
                }
                if (!(tabProperties instanceof TabJoinProperties)) {
                    throw new SqlProviderException("can not cast join field id: " + properties.joinFieldId);
                }
                TabJoinProperties joinProperties = (TabJoinProperties)tabProperties;
                properties.tabAlias = joinProperties.tabAlias;
                sessionContainer.propertiesMap.put(properties.fieldId, properties);
            }
        }
        for (TabField tabField : sessionContainer.mapping.fieldMap.values()) {
            if (sessionContainer.propertiesMap.containsKey(tabField.fieldName)) continue;
            TabCol tabCol = (TabCol)tabField.annotationMap.get(TabCol.class);
            TabProperties properties = new TabProperties();
            if (null == tabCol) {
                properties.col = camelCaseToSqlCase(tabField.fieldName);
            } else {
                properties.col = tabCol.value();
            }
            properties.tabAlias = sessionContainer.tabAlias;
            properties.fieldId = tabField.fieldName;
            sessionContainer.propertiesMap.put(properties.fieldId, properties);
        }
    }
    
    private void parseQuery() {
        Object query = sessionContainer.query.val;
        Class<?> clz = query.getClass();
        List<Field> fieldList = getAllFieldsList(clz);
        for (Field field : fieldList) {
            TabField tabField = new TabField();
            tabField.field = field;
            tabField.fieldName = field.getName();
            tabField.annotationMap = new HashMap<>();
            sessionContainer.mapping.fieldMap.put(tabField.fieldName, tabField);
            Annotation[] annotations = field.getAnnotations();
            if (null != annotations) {
                for (Annotation annotation : annotations) {
                    Class<? extends Annotation> annotationClz = annotation.annotationType();
                    tabField.annotationMap.put(annotationClz, annotation);
                    List<TabField> tableFields = sessionContainer.query.annotationFieldMap.get(annotationClz);
                    if (null == tableFields) {
                        tableFields = new ArrayList<>();
                        sessionContainer.query.annotationFieldMap.put(annotationClz, tableFields);
                    }
                    tableFields.add(tabField);
                }
            }
        }
        parseQueryField();
        parseCondition();
        parseOrderBy();
        parsePageLimit();
    }
    
    private void parseQueryField() {
        Object query = sessionContainer.query.val;
        List<TabField> queryFieldList = sessionContainer.query.annotationFieldMap.get(QueryField.class);
        for (TabField tabField : queryFieldList) {
            Field field = tabField.field;
            Object fieldValue = getFieldValue(field, query);
            if (null == fieldValue) continue;
            QueryField queryField = (QueryField) tabField.annotationMap.get(QueryField.class);
            ValueQueryCondition valueQueryCondition = new ValueQueryCondition();
            valueQueryCondition.setFieldId("".equals(queryField.fieldId()) ? field.getName() : queryField.fieldId());
            valueQueryCondition.setOp(null == queryField.op() ? ConditionOp.EQ.getCode() : queryField.op().getCode());
            valueQueryCondition.setValue(fieldValue);
            sessionContainer.conditionList.add(valueQueryCondition);
        }
    }

    private void parseCondition() {
        Object query = sessionContainer.query.val;
        List<TabField> queryFieldList = sessionContainer.query.annotationFieldMap.get(Condition.class);
        for (TabField tabField : queryFieldList) {
            Field field = tabField.field;
            Object fieldValue = getFieldValue(field, query);
            if (null == fieldValue) continue;
            if (fieldValue instanceof List) {
                List fieldValueList = (List) fieldValue;
                if (fieldValueList.isEmpty()) {
                    continue;
                }
                for (int i1 = 0; i1 < fieldValueList.size(); i1++) {
                    Object item = fieldValueList.get(i1);
                    if (item instanceof QueryCondition) {
                        QueryCondition queryCondition = (QueryCondition) item;
                        sessionContainer.conditionList.add(queryCondition);
                    } else {
                        throw new SqlProviderException("condition Value must QueryCondition");
                    }
                }
            } else {
                if (fieldValue instanceof QueryCondition) {
                    sessionContainer.conditionList.add(((QueryCondition) fieldValue).clone());
                } else {
                    throw new SqlProviderException("condition Value must QueryCondition");
                }
            }
        }
    }

    private void parseOrderBy() {
        List<TabField> tabFieldList = sessionContainer.query.annotationFieldMap.get(OrderBy.class);
        if (null != tabFieldList) {
            for (TabField tabField : tabFieldList) {
                Object fieldValue = ReflectUtil.getFieldValue(tabField.field, sessionContainer.query.val);
                List<PageOrderBy> orderByList = builderPageOrderByList(fieldValue);
                sessionContainer.orderByList.addAll(orderByList);
            }
        }
        
    }

    /**
     * sort
     * @param fieldValue
     * @return
     */
    private List<PageOrderBy> builderPageOrderByList(Object fieldValue) {
        List<PageOrderBy> orderByList = new ArrayList<>();
        if (null == fieldValue) {
            return orderByList;
        }
        if (fieldValue instanceof Collection) {
            Collection fieldValueCollection = (Collection) fieldValue;
            for (Object o : fieldValueCollection) {
                PageOrderBy pageOrderBy = getPageOrderBy(o);
                if (null != pageOrderBy) {
                    orderByList.add(pageOrderBy);
                }
            }
        } else {
            PageOrderBy pageOrderBy = getPageOrderBy(fieldValue);
            if (null != pageOrderBy) {
                orderByList.add(pageOrderBy);
            }
        }
        return orderByList;
    }

    private PageOrderBy getPageOrderBy(Object o) {
        if (null == o) {
            return null;
        }
        Field declaredField = ReflectUtil.getClsField("fieldId", o.getClass());
        if (null == declaredField) {
            return null;
        }
        Object childFieldValue = ReflectUtil.getFieldValue(declaredField, o);
        if (null == childFieldValue) {
            return null;
        }
        PageOrderBy pageOrderBy = new PageOrderBy();
        pageOrderBy.setFieldId(childFieldValue.toString());
        Field typeField = ReflectUtil.getClsField("type", o.getClass());
        if (null != typeField) {
            Object typeValue = ReflectUtil.getFieldValue(typeField, o);
            if (null != typeValue) {
                pageOrderBy.setType(typeValue.toString());
            }
        }
        return pageOrderBy;
    }


    private void parsePageLimit() {
        List<TabField> pageSizeFieldList = sessionContainer.query.annotationFieldMap.get(PageSize.class);
        if (null == pageSizeFieldList) {
            return;
        }
        PageLimit pageLimit = new PageLimit();
        for (int i = 0; i < pageSizeFieldList.size() && i < 1; i++) {
            TabField tabField = pageSizeFieldList.get(i);
            Object fieldValue = ReflectUtil.getFieldValue(tabField.field, sessionContainer.query.val);
            if (null != fieldValue) {
                if (fieldValue instanceof Integer) {
                    pageLimit.pageSize = (Integer)fieldValue;
                } else if (fieldValue instanceof Long) {
                    pageLimit.pageSize = ((Long)fieldValue).intValue();
                } else {
                    pageLimit.pageSize = Integer.valueOf(fieldValue.toString());
                }
            }
        }
        if (null == pageLimit.pageSize) {
            return ;
        }
        List<TabField> pageIndexFieldList = sessionContainer.query.annotationFieldMap.get(PageIndex.class);
        sessionContainer.pageLimit = pageLimit;
        for (int i = 0; i < pageIndexFieldList.size() && i < 1; i++) {
            TabField tabField = pageIndexFieldList.get(i);
            Object fieldValue = ReflectUtil.getFieldValue(tabField.field, sessionContainer.query.val);
            if (null != fieldValue) {
                if (fieldValue instanceof Integer) {
                    pageLimit.pageIdx = (Integer)fieldValue;
                } else if (fieldValue instanceof Long) {
                    pageLimit.pageIdx = ((Long)fieldValue).intValue();
                } else {
                    pageLimit.pageIdx = Integer.valueOf(fieldValue.toString());
                }
            }
        }
    }
    
    
    private String getSelectSql() {
        StringBuilder sbu = new StringBuilder();
        sessionContainer.propertiesMap.forEach((fieldName, properties) -> {
            if (sbu.length() > 0) {
                sbu.append(", ");
            }
            sbu
                .append(properties.tabAlias)
                .append(".")
                .append(symbol)
                .append(properties.col)
                .append(symbol)
                .append(" as ")
                .append(symbol)
                .append(fieldName)
                .append(symbol);
        });
        sbu.insert(0, "select ").append("\n");
        return sbu.toString();
    }
    
    private String getFormSql() {
        StringBuilder formBuilder = new StringBuilder();
        formBuilder.append("from ")
                .append(symbol)
                .append(sessionContainer.tabName)
                .append(symbol)
                .append(" as ").append(sessionContainer.tabAlias).append("\n");
        return formBuilder.toString();
    }

    private String getJoinName(int joinType) {
        String joinName = null;
        switch (joinType) {
            case 0:
                joinName = " left join "; break;
            case 1:
                joinName = " right join "; break;
            case 2:
                joinName = " inner join "; break;
        }
        return joinName;
    }
    
    private String getJoinSql() {
        StringBuilder sbu = new StringBuilder();
        sessionContainer.joinPropertiesMap.forEach((fieldName, properties) -> 
                                                sbu.append(buildJoinSql(properties)));
        return sbu.toString();
    }

    private String buildJoinSql(TabJoinProperties properties) {
        return buildJoinSql(null, properties);
    }
    
    private String buildJoinSql(TabJoinProperties leftProperties, TabJoinProperties properties) {
        StringBuilder sbu = new StringBuilder();
        sbu.append(getJoinName(properties.joinType))
                .append(symbol)
                .append(properties.tabName)
                .append(symbol)
                .append(" as ")
                .append(properties.tabAlias)
                .append(" on ")
        ;
        String leftAlias = null == leftProperties ? sessionContainer.tabAlias : leftProperties.tabAlias;
        for (int i = 0; i < properties.onValueList.size(); i++) {
            String onValue = properties.onValueList.get(i);
            if (i > 0) {
                sbu.append(" and ");
            }
            sbu.append("(");
            Map<String, Object> variables = new HashMap<>();
            variables.put("leftAlias", leftAlias);
            variables.put("rightAlias", properties.tabAlias);
            ElContextExecutor elContextExecutor = ElContextExecutor.create(variables);
            String subWhereSql = parseElValues(onValue, (val) -> elContextExecutor.getValue(val, String.class));
            sbu.append(subWhereSql).append(")\n");
        }
        for (TabJoinProperties joinProperties : properties.subJoinList) {
            sbu.append("\r").append(
                    buildJoinSql(properties, joinProperties)
            );
        }
        return sbu.toString();
    }

    private String parseElValues(String value, Function<String, String> fieldGetFunc) {
        char[] chars = value.toCharArray();
        boolean open = false;
        StringBuilder strBuilder = new StringBuilder();
        StringBuilder sbuBuilder = new StringBuilder();
        for (int i = 0; i < chars.length; i++) {
            char c = chars[i];
            if (open) {
                if (125 == c) {
                    String apply = fieldGetFunc.apply("${" + sbuBuilder + "}");
                    strBuilder.append(apply);
                    sbuBuilder.setLength(0);
                    open = false;
                } else {
                    sbuBuilder.append(c);
                }
            } else {
                if (36 == c && i != chars.length -1 && 123 == chars[i + 1]) {
                    open = true;
                    ++ i;
                } else {
                    strBuilder.append(c);
                }
            }
        }
        return strBuilder.toString();
    }


    private String getWhereSql () {
        List<QueryCondition> conditionList = sessionContainer.conditionList;
        StringBuilder whereBuilder = new StringBuilder();
        for (QueryCondition queryCondition : conditionList) {
            String querySql = getQuerySql(queryCondition, sessionContainer.propertiesMap);
            if (!querySql.isEmpty()) {
                if (whereBuilder.length() > 0) {
                    whereBuilder.append(" and ");
                }
                whereBuilder.append(querySql);
            }
        }
        if (0 != whereBuilder.length()) {
            whereBuilder.insert(0, "where\n\t").append("\n");
        }
        return whereBuilder.toString();
    }

    private String getQuerySql(QueryCondition queryCondition, Map<String, TabProperties> propertiesMap) {
        StringBuilder whereBuilder = new StringBuilder();
        String conditionSql = getConditionSql(queryCondition, propertiesMap);
        if (!conditionSql.isEmpty()) {
            whereBuilder.append(conditionSql);
        }
        QueryFilter filter = queryCondition.getFilter();
        if (null == filter) {
            return whereBuilder.toString();
        }
        String logicalOperator = "and";
        String filterOpValue = filter.getOp();
        if (null != filterOpValue) {
            if ("and".equalsIgnoreCase(filterOpValue)) {
                logicalOperator = "and";
            } else if ("or".equalsIgnoreCase(filterOpValue)) {
                logicalOperator = "or";
            }
        }
        List<QueryCondition> children = filter.getChildren();
        if (null == children) {
            return whereBuilder.toString();
        }
        if (whereBuilder.length() > 0 ) {
            whereBuilder.append(" and ");
        }
        whereBuilder.append(" (\n\t\t");
        for (int i = 0; i < children.size(); i++) {
            QueryCondition childBean = children.get(i);
            String childSql = getQuerySql(childBean, propertiesMap);
            if (!childSql.isEmpty()) {
                if (i > 0) {
                    whereBuilder.append(" ").append(logicalOperator).append(" ");
                }
                whereBuilder.append(childSql);
            }
        }
        whereBuilder.append("\n\t\t ) ");
        return whereBuilder.toString();
    }

    private String getConditionSql(QueryCondition queryCondition, Map<String, TabProperties> propertiesMap) {
        if (null == queryCondition) {
            return "";
        }
        if (queryCondition instanceof SqlCondition) {
            SqlCondition sqlCondition = (SqlCondition) queryCondition;
            return "(" + sqlCondition.getSql() + ")";
        }
        StringBuilder sqlBuilder = new StringBuilder();
        String fieldId = queryCondition.getFieldId();
        if (null == fieldId || fieldId.isEmpty()) {
            return "";
        }
        String conditionOp = queryCondition.getOp();
        ConditionOp op;
        if (null == conditionOp) {
            // default eq
            op = ConditionOp.EQ;
        } else {
            op = ConditionOp.getByOp((conditionOp));
            if (null == op) {
                throw new SqlProviderException("unknown condition op: " + conditionOp);
            }
        }
        TabProperties properties = propertiesMap.get(fieldId);
        if (null == properties) {
            throw new SqlProviderException("unknown fieldId in fieldList :" + fieldId);
        }
        Object conditionValue = queryCondition.getValue();
        String columnName;
        columnName = properties.tabAlias + "." +  symbol + properties.col + symbol;
        if (ConditionOp.NE.equals(op) || ConditionOp.NOTIN.equals(op)) {
            String notNullWhereSql = getConditionValueSql(properties, ConditionOp.ISNULL, conditionValue);
            String conditionValueWhereSql = getConditionValueSql(properties, op, conditionValue);
            sqlBuilder.append("(").append(columnName).append(notNullWhereSql)
                    .append(" or ")
                    .append(columnName).append(conditionValueWhereSql)
                    .append(")");
        } else {
            String conditionValueWhereSql = getConditionValueSql(properties, op, conditionValue);
            sqlBuilder.append(columnName).append(conditionValueWhereSql);
        }
        ++properties.showCount;

        return sqlBuilder.toString();
    }

    private String getConditionValueSql(TabProperties field, ConditionOp op, Object value) {
        StringBuilder sqlBuilder = new StringBuilder();
        switch (op) {
            case NOTNULL:
                sqlBuilder.append(" is not null ");
                break;
            case ISNULL:
                sqlBuilder.append(" is null ");
                break;
            case IN:
                sqlBuilder.append(" in ");
                if (!(value instanceof Collection<?>)) {
                    throw new SqlProviderException(" when op is in, value must be collection");
                }
                Collection coll = (Collection) value;
                sqlBuilder.append("(");
                int idx = 0;
                for (Object o : coll) {
                    if (idx > 0) {
                        sqlBuilder.append(", ");
                    }
                    sqlBuilder.append("?");
                    sessionContainer.paramList.add(o);
                    ++idx;
                }
                sqlBuilder.append(")");
                break;
            case NOTIN:
                sqlBuilder.append(" not in ");
                if (!(value instanceof Collection<?>)) {
                    throw new SqlProviderException(" when op is in, value must be collection");
                }
                Collection coll2 = (Collection) value;
                sqlBuilder.append("(");
                int idx2 = 0;
                for (Object o : coll2) {
                    if (idx2 > 0) {
                        sqlBuilder.append(", ");
                    }
                    sqlBuilder.append("?");
                    sessionContainer.paramList.add(o);
                    ++idx2;
                }
                sqlBuilder.append(")");
                break;
            case E_NOTIN:
                sqlBuilder.append(" not in ");
                if (!(value instanceof Collection<?>)) {
                    throw new SqlProviderException(" when op is in, value must be collection");
                }
                Collection colle = (Collection) value;
                sqlBuilder.append("(");
                int idx2e = 0;
                for (Object o : colle) {
                    if (idx2e > 0) {
                        sqlBuilder.append(", ");
                    }
                    sqlBuilder.append("?");
                    sessionContainer.paramList.add(o);
                    ++idx2e;
                }
                sqlBuilder.append(")");
                break;
            case EQ:
                sqlBuilder.append("=?");
                sessionContainer.paramList.add(value);
                break;
            case NE:
                sqlBuilder.append("<>?");
                sessionContainer.paramList.add(value);
                break;
            case E_NE:
                sqlBuilder.append("<>?");
                sessionContainer.paramList.add(value);
                break;
            case LT:
                sqlBuilder.append("<?");
                sessionContainer.paramList.add(value);
                break;
            case LE:
                sqlBuilder.append("<=?");
                sessionContainer.paramList.add(value);
                break;
            case GT:
                sqlBuilder.append(">?");
                sessionContainer.paramList.add(value);
                break;
            case GE:
                sqlBuilder.append(">=?");
                sessionContainer.paramList.add(value);
                break;
            case BETWEEN:
                if (!(value instanceof Collection<?>)) {
                    throw new SqlProviderException(" when op is between, value must be collection");
                }
                sqlBuilder.append("between ? and ?");
                Collection coll3 = (Collection) value;
                int idx3 = 0;
                for (Object o : coll3) {
                    sqlBuilder.append("?");
                    sessionContainer.paramList.add(o);
                    ++idx3;
                    if (idx3 > 1) {
                        break;
                    }
                }
                sessionContainer.paramList.add(value);
                break;
            case LIKE:
                sqlBuilder.append(" like concat('%', ?, '%')");
                sessionContainer.paramList.add(value);
                break;
            case LEFT_LIKE:
                sqlBuilder.append(" like concat(?, '%')");
                sessionContainer.paramList.add(value);
                break;
            case RIGHT_LIKE:
                sqlBuilder.append(" like concat('%', ?)");
                sessionContainer.paramList.add(value);
                break;
        }
        return sqlBuilder.toString();
    }

    private String getOrderBySql() {
        //query查询 获取limit数据
        List<PageOrderBy> orderByList = sessionContainer.orderByList;
        if (null == orderByList || orderByList.isEmpty()) {
            return "";
        }
        StringBuilder sbu = new StringBuilder("order by \n   ");
        for (int i = 0; i < orderByList.size(); i++) {
            PageOrderBy pageOrderBy = orderByList.get(i);
            if (i > 0) {
                sbu.append(", ");
            }
            TabProperties fieldProperties = sessionContainer.propertiesMap.get(pageOrderBy.getFieldId());
            if (null == fieldProperties) {
                throw new SqlProviderException("unknown pageOrderBy.fieldId  【" + pageOrderBy.getFieldId() + "】");
            }
            ++fieldProperties.showCount;
            sbu.append(fieldProperties.tabAlias)
                    .append(".").append(symbol)
                    .append(fieldProperties.col).append(symbol);
            String sqlType = OrderByType.getOrderByType(pageOrderBy.getType());
            if (null != sqlType) {
                sbu.append(" ")
                        .append(sqlType);
            }
        }
        return sbu.append("\n").toString();
    }
    
    /**
     * get page limit
     * @return
     */
    private String getLimitSql() {
        if (null == sessionContainer.pageLimit) {
            return "";
        }
        // only have pageSize
        if (null == sessionContainer.pageLimit.pageIdx) {
            sessionContainer.paramList.add(sessionContainer.pageLimit.pageSize);
            return "limit ?";
        }
        sessionContainer.paramList.add(sessionContainer.pageLimit.pageSize);
        sessionContainer.paramList.add(getPageIndex(sessionContainer.pageLimit.pageIdx, sessionContainer.pageLimit.pageSize));
        return "limit ? offset ?";
    }

    public Integer getPageIndex(Integer pageIndex, Integer pageSize) {
        if (null == pageIndex) {
            return null;
        }
        if (null == pageSize) {
            return null;
        }
        return (pageIndex-1) * pageSize;
    }
    
    public SqlProviderBuilder query() {
        StringBuilder sbu = new StringBuilder();
        sbu.append(getSelectSql())
                .append(getFormSql())
                .append(getJoinSql())
                .append(getWhereSql());
        if (null != sessionContainer.endConditionParamList) {
            sessionContainer.paramList.addAll(sessionContainer.endConditionParamList);
        }
        sbu.append(getOrderBySql())
                .append(getLimitSql());
        sessionContainer.sql = sbu.toString();
        return this;
    }
    
    public SqlProviderBuilder queryCount() {
        StringBuilder sbu = new StringBuilder();
        sbu.append("select count(1) ")
                .append(getFormSql())
                .append(getJoinSql())
                .append(getWhereSql());
        sessionContainer.sql = sbu.toString();
        if (null != sessionContainer.endConditionParamList) {
            sessionContainer.paramList.addAll(sessionContainer.endConditionParamList);
        }
        return this;
    }
    
    
    public SessionContainer result() {
        return sessionContainer;
    }
    
    
}
