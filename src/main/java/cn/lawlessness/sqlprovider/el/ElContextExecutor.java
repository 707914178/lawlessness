package cn.lawlessness.sqlprovider.el;

import org.apache.el.lang.ExpressionBuilder;
import org.apache.el.parser.Node;

import javax.el.ELContext;
import javax.el.ExpressionFactory;
import javax.el.PropertyNotFoundException;
import javax.el.StandardELContext;
import javax.el.ValueExpression;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;

/**
 * @author liutao
 * @date 2024/11/27
 */
public class ElContextExecutor {

    private int variableCount = 0;
    private ExpressionFactory factory;
    private ELContext context;

    public void setContext(ELContext context) {
        this.context = context;
    }
    public void setFactory(ExpressionFactory factory) {
        this.factory = factory;
    }

    public <T> T getValue(String expression, Class<T> clz) {
        if (0 == variableCount) {
            return null;
        }
        ValueExpression valueExpression = this.factory.createValueExpression(context, expression, clz);
        try {
            return (T)valueExpression.getValue(this.context);
        } catch (PropertyNotFoundException notFoundException){
            return null;
        }
    }

    public Object getValue(String expression) {
        if (0 == variableCount) {
            return null;
        }
        ValueExpression valueExpression = this.factory.createValueExpression(context, expression, Object.class);
        try {
            return valueExpression.getValue(this.context);
        } catch (PropertyNotFoundException notFoundException){
            return null;
        }
    }

    public static Set<String> keys(String expression) {
        Set<String> keys = new LinkedHashSet<>();
        Node node = ExpressionBuilder.createNode(expression);
        String image = node.getImage();
        if (null != image) {
            keys.add(image);
            return keys;
        }
        int i1 = node.jjtGetNumChildren();
        for (int i = 0; i < i1; i++) {
            Node childNode = node.jjtGetChild(i);
            // un expression
            if (null != childNode.getImage() || childNode.jjtGetNumChildren() <= 0) {
                continue;
            }
            String childImage = getNodeImage(childNode);
            if (null != childImage) {
                keys.add(childImage);
            }
        }
        return keys;
    }

    private static String getNodeImage(Node node) {
        if (null == node) {
            return null;
        }
        String image = node.getImage();
        if (null != image) {
            return image;
        }
        if (node.jjtGetNumChildren() > 0) {
            return getNodeImage(node.jjtGetChild(0));
        }
        return null;
    }

    public static ElContextExecutor create(Map<String, ?> variableContext) {
        // 创建ExpressionFactory
        ExpressionFactory factory = ExpressionFactory.newInstance();
        // 创建一个ELContext
        ELContext context = new StandardELContext(factory);
        if (null == variableContext) {
            variableContext = Collections.EMPTY_MAP;
        }
        Set<? extends Entry<String, ?>> entries = variableContext.entrySet();
        for (Entry<String, ?> entry : entries) {
            String key = entry.getKey();
            Object value = entry.getValue();
            if (null == value) {
                context.getVariableMapper().setVariable(key, factory.createValueExpression(null, Object.class));
            } else {
                context.getVariableMapper().setVariable(key, factory.createValueExpression(value, value.getClass()));
            }
        }
        ElContextExecutor elContextExecutor = new ElContextExecutor();
        elContextExecutor.context = context;
        elContextExecutor.factory = factory;
        elContextExecutor.variableCount = variableContext.size();
        return elContextExecutor;
    }

    public static Set<String> keys(String key, String expression) {
        Map<String, Object> variables = new HashMap<>();
        ElKeyAcquireMap<String, Object> keyVariables = new ElKeyAcquireMap<>();
        variables.put(key, keyVariables);
        ElContextExecutor elContextExecutor = ElContextExecutor.create(variables);
        parseElValues(expression, (val) -> {
            elContextExecutor.getValue(val);
            return null;
        });
        return keyVariables.keys;
    }

    public static class ElKeyAcquireMap<K,V> extends HashMap<K,V> implements Serializable {
        private static final long serialVersionUID = 7494488016584681552L;
        public Set<K> keys = new HashSet<>();
        @Override
        public V get(Object key) {
            keys.add((K)key);
            return null;
        }

        @Override
        public boolean containsKey(Object key) {
            return super.containsKey(key);
        }
    }

    public static String parseElValues(String value, Function<String, String> fieldGetFunc) {
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
}
