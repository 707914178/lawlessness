package cn.lawlessness.sqlprovider.utils;

import cn.lawlessness.sqlprovider.excpetion.SqlProviderException;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author liutao
 * @date 2023/9/18 18:22
 */
public class ReflectUtil {

    public static List<Field> getAllFieldsList(final Class<?> cls) {
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
            if (Object.class.equals(currentClass)) {
                break;
            }
        }
        final List<Field> allFields = new ArrayList<>();
        for (int i = fieldsList.size()-1; i >= 0; i--) {
            List<Field> fields = fieldsList.get(i);
            allFields.addAll(fields);
        }
        return allFields;
    }


    public static Class<?> getClassType(Type genericType) {
        if (genericType instanceof Class<?>) {
            return (Class<?>) genericType;
        }
        // if is list
        else if (genericType instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) genericType;
            Type rawType = parameterizedType.getRawType();
            if (null == rawType) {
                return null;
            }
            if (rawType instanceof Class<?>) {
                return (Class<?>) rawType;
            }
        }
        return null;
    }

    public static Class<?> getListClassType(Type genericType) {
        if (genericType instanceof Class<?>) {
            return (Class<?>) genericType;
        }
        // if is list
        else if (genericType instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) genericType;
            // 获取 List 的泛型参数类型
            Type[] typeArguments = parameterizedType.getActualTypeArguments();
            if (typeArguments.length > 0) {
                Class<?> elementType = (Class<?>) typeArguments[0];
                return elementType;
            }
        }
        return null;
    }

    public static List<Field> getHasAnnotationFieldList(Class<?> clz, Class<? extends Annotation> annotationClz) {
        List<Field> allFieldsList = getAllFieldsList(clz);
        List<Field> fieldList = new ArrayList<>();
        for (Field field : allFieldsList) {
            Annotation annotation = field.getAnnotation(annotationClz);
            if (null != annotation) {
                fieldList.add(field);
            }
        }
        return fieldList;
    }


    public static <T extends Annotation> List<T> getFieldAnnotationList(Class<?> clz, Class<T> annotationClz) {
        List<Field> allFieldsList = getAllFieldsList(clz);
        List<T> annotationList = new ArrayList<>();
        for (Field field : allFieldsList) {
            T annotation = field.getAnnotation(annotationClz);
            if (null != annotation) {
                annotationList.add(annotation);
            }
        }
        return annotationList;
    }


    public static boolean isFinalOrStaticField (Field field) {
        if (Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers())) {
            return true;
        }
        return false;
    }

    public static Object getFieldValue(String fieldName, Object object) {
        Field declaredField = getClsField(fieldName, object.getClass());
        if (null == declaredField) {
            throw new SqlProviderException("fieldName not found: " + fieldName + ", object.class:" + object.getClass().getName());
        }
        return getFieldValue(declaredField, object);
    }

    public static Field getClsField(String fieldName, Class<?> cls) {
        if (null == cls) {
            throw new SqlProviderException("cls is null");
        }
        if (null == fieldName || "".equals(fieldName)) {
            throw new SqlProviderException("fieldName is empty");
        }
        try {
            return cls.getDeclaredField(fieldName);
        } catch (NoSuchFieldException ne) {
            // maybe field in parent clz
            Class<?> superclass = cls.getSuperclass();
            if (Object.class.equals(superclass)) {
                return null;
            } else {
                return getClsField(fieldName, superclass);
            }
        }
    }

    public static Object getFieldValue(Field field, Object object) {
        try {
            String name = field.getName();
            String methodName = new StringBuilder("get")
                    .append(String.valueOf(name.charAt(0)).toUpperCase())
                    .append(name.substring(1))
                    .toString();
            Method method = object.getClass().getMethod(methodName);
            Object invoke = method.invoke(object);
            return invoke;
        } catch (NoSuchMethodException e) {
        } catch (IllegalAccessException e) {
        } catch (InvocationTargetException e) {
        }
        Object val = null;
        field.setAccessible(true);
        try {
            val = field.get(object);
        } catch (IllegalAccessException e) {
        }
        field.setAccessible(false);
        return val;
    }



}
