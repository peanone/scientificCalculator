//! BATU ARIBAKIR 220403022
//* Assoc. Prof. Volkan KILIC
//* Artificial Intelligence Laboratory
//* Scientific Calculator
//* 29.03.2024

/*
(a,b,c,d,f,g,h,i (lowercase) are random variables for the examples.)
(A,B,C,D,E,...,Z (UPPERCASE) are variables to hold values.)

The functions:
+, -, /, *, !, ^, e, pi, ANS, abs(a), mod(a,b), rnd(a), sin(a), cos(a), tan(a), cot(a), sec(a), csc(a), asin(a), acos(a), atan(a), acot(a), asec(a), acsc(a), log(a,b), ln(a)*/
/*
! perm(a,b) -> Permutation,
! comb(a,b) -> Combination,
! pyt(a,b) -> Pisagor Theorem,
! pol(a,b) -> Polar representation (z= a(cos(b)+isin(b))),
! STO(A) -> Stores ANS into 'A' variable,
! quit -> Exiting program
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#define SUCCESS 0
#define ERROR_DIVISION_BY_ZERO 1
#define ERROR_UNKNOWN_FUNCTION 2
#define ERROR_UNEXPECTED_CHARACTER 3
#define ERROR_INCOMPLETE_EXPRESSION 4
#define MAX_VARIABLES 26 // maximum number of variables (A-Z)

typedef struct
{
    const char *str;
    size_t pos;
    int ch;
    size_t length;
} Parser;

typedef struct
{
    double r;     // Magnitude
    double theta; // Angle in radians
} Polar;

// Function to convert rectangular coordinates to polar coordinates
Polar pol(double x, double y)
{
    Polar polar;
    polar.r = sqrt(x * x + y * y);
    polar.theta = atan2(y, x);
    return polar;
}

// function to initialize the parser
void init_parser(Parser *p, const char *str)
{
    p->str = str;
    p->pos = 0;
    p->ch = str[0];
    p->length = strlen(str);
}

void next_char(Parser *p)
{
    p->ch = (++p->pos < p->length) ? p->str[p->pos] : '\0';
}

int parse_expression(Parser *p, double *result, double prev_result, double stored_variables[]);
int parse_term(Parser *p, double *result, double prev_result, double stored_variables[]);
int parse_factor(Parser *p, double *result, double prev_result, double stored_variables[]);

// function to check if the current character matches a given character and consume it if it does
int eat(Parser *p, int charToEat)
{
    if (p->ch == charToEat)
    {
        next_char(p);
        return 1;
    }
    return 0;
}
double permutation(int n, int r)
{
    if (n < r)
        return 0;
    double result = 1;
    for (int i = n; i > n - r; --i)
    {
        result *= i;
    }
    return result;
}
double combination(int n, int r)
{
    if (n < r)
        return 0;
    double result = 1;
    for (int i = 0; i < r; ++i)
    {
        result *= (n - i);
        result /= (i + 1);
    }
    return result;
}

// function to parse the input expression and evaluate it
int eval(const char *str, double *result, double *prev_result, double stored_variables[])
{
    Parser p;
    init_parser(&p, str);

    int status = parse_expression(&p, result, *prev_result, stored_variables);

    if (status != SUCCESS)
        return status;

    if (p.ch != '\0')
        return ERROR_INCOMPLETE_EXPRESSION;

    // update previous result only if the evaluation was successful
    *prev_result = *result;

    return SUCCESS;
}

// function to parse an expression
int parse_expression(Parser *p, double *result, double prev_result, double stored_variables[])
{
    double x;

    int status = parse_term(p, &x, prev_result, stored_variables);
    if (status != SUCCESS)
        return status;

    if (eat(p, '!'))
    {
        // Factorial is only defined for non-negative integers
        if (x < 0 || x != (int)x)
            return ERROR_UNEXPECTED_CHARACTER;

        // Calculate the factorial
        int n = (int)x;
        double factorial = 1;
        for (int i = 2; i <= n; ++i)
            factorial *= i;
        x = factorial;
    }

    while (p->ch == '+' || p->ch == '-')
    {
        char op = p->ch;
        next_char(p);

        if (!isdigit(p->ch) && p->ch != '.' && !isalpha(p->ch) && p->ch != '(' && p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        double y;
        status = parse_term(p, &y, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (op == '+')
            x += y;
        else
            x -= y;
    }

    *result = x;
    return SUCCESS;
}

// function to parse a term
int parse_term(Parser *p, double *result, double prev_result, double stored_variables[])
{
    double x;

    int status = parse_factor(p, &x, prev_result, stored_variables);
    if (status != SUCCESS)
        return status;

    while (p->ch == '*' || p->ch == '/' || p->ch == '^')
    {
        if (p->ch == '*')
        {
            next_char(p);
            double y;
            status = parse_factor(p, &y, prev_result, stored_variables);
            if (status != SUCCESS)
                return status;
            x *= y;
        }
        else if (p->ch == '/')
        {
            next_char(p);
            double y;
            status = parse_factor(p, &y, prev_result, stored_variables);
            if (status != SUCCESS)
                return status;
            if (y == 0)
                return ERROR_DIVISION_BY_ZERO;
            x /= y;
        }
        else if (p->ch == '^')
        {
            next_char(p); // move past '^'
            double exponent;
            status = parse_term(p, &exponent, prev_result, stored_variables); // parse the exponent
            if (status != SUCCESS)
                return status;
            x = pow(x, exponent);
        }
    }

    *result = x;

    return SUCCESS;
}

// function to parse a factor
int parse_factor(Parser *p, double *result, double prev_result, double stored_variables[])
{
    double x = 0;

    if (eat(p, '+'))
    {
        int status = parse_factor(p, &x, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
    }
    else if (eat(p, '-'))
    {
        int status = parse_factor(p, &x, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        x = -x;
    }
    else if (eat(p, '('))
    {
        int status = parse_expression(p, &x, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (isdigit(p->ch) || p->ch == '.')
    {
        char num_str[50];
        int start_pos = p->pos;
        while (isdigit(p->ch) || p->ch == '.')
        {
            next_char(p);
        }
        strncpy(num_str, &(p->str[start_pos]), p->pos - start_pos);
        num_str[p->pos - start_pos] = '\0';
        x = atof(num_str);
    }
    else if (p->ch == 'e')
    {
        x = exp(1);
        next_char(p); // move past 'e'
    }
    else if (p->ch == 'p' && p->pos + 1 < p->length && p->str[p->pos + 1] == 'i')
    {
        x = M_PI;     // Assign the value of pi to the result
        next_char(p); // move past 'p'
        next_char(p); // move past 'i'
    }

    else if (strncmp(p->str + p->pos, "ANS", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        x = prev_result;
    }
    else if (p->ch == '^')
    {

        next_char(p);
        double exponent;
        int status = parse_factor(p, &exponent, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        x = pow(x, exponent);
    }
    else if (strncmp(p->str + p->pos, "abs", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        int status = parse_expression(p, &x, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
        x = fabs(x); // calculate absolute value using fabs function
    }
    else if (strncmp(p->str + p->pos, "mod", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');

        double dividend;
        int status = parse_expression(p, &dividend, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ','

        double divisor;
        status = parse_expression(p, &divisor, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ')'

        // calculate the modulo operation
        if (divisor == 0)
            return ERROR_DIVISION_BY_ZERO;
        else
            x = fmod(dividend, divisor);
    }
    else if (strncmp(p->str + p->pos, "rnd", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        x = round(arg);
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "pyt", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');

        double a;
        int status = parse_expression(p, &a, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ','

        double b;
        status = parse_expression(p, &b, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ')'

        x = sqrt(a * a + b * b);
    }
    else if (strncmp(p->str + p->pos, "pol", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double real_part;
        int status = parse_expression(p, &real_part, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;
        next_char(p); // move past ','
        double imag_part;
        status = parse_expression(p, &imag_part, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;
        next_char(p); // move past ')'
        Polar polar = pol(real_part, imag_part);
        printf("Polar form of %.1lf + %.1lfi: r = %.2lf, theta = %.2lf radians\n", real_part, imag_part, polar.r, polar.theta);
        x = polar.r; // Return the magnitude as the result
    }
    else if (strncmp(p->str + p->pos, "sin", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        x = sin(arg * M_PI / 180.0);
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "cos", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        x = cos(arg * M_PI / 180.0);
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }

    else if (strncmp(p->str + p->pos, "tan", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        // check if the angle is close to a singularity
        if (fmod(arg, 180.0) == 90.0)
        {
            // return a special value to indicate undefined tangent
            x = NAN; // or INFINITY
        }
        else
        {
            x = tan(arg * M_PI / 180.0);
        }
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "cot", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the angle is close to a singularity
        if (fmod(arg, 180.0) == 0.0)
        {

            x = NAN; // or INFINITY
        }
        else
        {
            x = 1.0 / tan(arg * M_PI / 180.0);
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "sec", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (fmod(arg, 90.0) == 0.0)
        {

            x = NAN; // or INFINITY
        }
        else
        {
            x = 1.0 / cos(arg * M_PI / 180.0);
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "csc", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (fmod(arg, 180.0) == 0.0)
        {

            x = NAN; // or INFINITY
        }
        else
        {
            x = 1.0 / sin(arg * M_PI / 180.0);
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }

    else if (strncmp(p->str + p->pos, "asin", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the argument is within the valid range [-1, 1]
        if (arg < -1.0 || arg > 1.0)
        {
            x = NAN; // or INFINITY
        }
        else
        {

            x = asin(arg) * 180.0 / M_PI;
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "acos", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the argument is within the valid range [-1, 1]
        if (arg < -1.0 || arg > 1.0)
        {

            x = NAN; // or INFINITY
        }
        else
        {

            x = acos(arg) * 180.0 / M_PI;
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "atan", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        x = atan(arg) * 180.0 / M_PI;

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "acot", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the argument is zero
        if (arg == 0.0)
        {

            x = NAN; // or INFINITY
        }
        else
        {

            x = atan(1.0 / arg) * 180.0 / M_PI;
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "asec", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the argument is within the valid range
        if (arg >= -1 && arg <= 1 && arg != 0)
        {
            x = acos(1 / arg) * 180.0 / M_PI;
        }
        else
        {

            x = NAN; // or INFINITY
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "acsc", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        // check if the argument is within the valid range
        if (arg >= -1 && arg <= 1 && arg != 0)
        {
            x = asin(1 / arg) * 180.0 / M_PI;
        }
        else
        {

            x = NAN; // or INFINITY
        }

        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "log", 3) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');

        double base;
        int status = parse_expression(p, &base, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ','

        double arg;
        status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ')'

        if (base <= 0 || arg <= 0)
            return ERROR_DIVISION_BY_ZERO; // Logarithm of non-positive number or invalid base
        else
            x = log(arg) / log(base);
    }
    else if (strncmp(p->str + p->pos, "ln", 2) == 0)
    {
        next_char(p);
        next_char(p);
        eat(p, '(');
        double arg;
        int status = parse_expression(p, &arg, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;
        if (arg <= 0)
            return ERROR_DIVISION_BY_ZERO;
        x = log(arg);
        if (!eat(p, ')'))
            return ERROR_UNEXPECTED_CHARACTER;
    }
    else if (strncmp(p->str + p->pos, "perm", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');

        // parse the total number of items
        double total;
        int status = parse_expression(p, &total, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ','

        // parse the number of items taken at a time
        double taken;
        status = parse_expression(p, &taken, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ')'

        x = permutation((int)total, (int)taken);
    }
    else if (strncmp(p->str + p->pos, "comb", 4) == 0)
    {
        next_char(p);
        next_char(p);
        next_char(p);
        next_char(p);
        eat(p, '(');

        // parse the total number of items
        double total;
        int status = parse_expression(p, &total, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ',')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ','

        // parse the number of items taken at a time
        double taken;
        status = parse_expression(p, &taken, prev_result, stored_variables);
        if (status != SUCCESS)
            return status;

        if (p->ch != ')')
            return ERROR_UNEXPECTED_CHARACTER;

        next_char(p); // move past ')'

        x = combination((int)total, (int)taken);
    }

    else if (isalpha(p->ch)) // to store the variables
    {
        // parse variable
        char variable = p->ch;
        next_char(p);
        if (isalpha(p->ch))
            return ERROR_UNKNOWN_FUNCTION; // invalid variable name (more than one character)
        if (!isupper(variable))
            return ERROR_UNKNOWN_FUNCTION; // invalid variable name (not uppercase)

        // check if it's a valid variable (A-Z)
        if (variable >= 'A' && variable <= 'Z')
        {
            // get the index of the variable in stored_variables array
            int index = variable - 'A';
            x = stored_variables[index]; // use the value stored in the variable
        }
        else
        {
            return ERROR_UNKNOWN_FUNCTION; // unknown variable
        }
    }
    else
    {
        return ERROR_UNEXPECTED_CHARACTER; // unexpected character
    }

    *result = x;
    return SUCCESS;
}

int main()
{
    char expression[100];
    double prev_result = 0;
    double stored_variables[MAX_VARIABLES] = {0}; // array to store variables A-Z

    while (1)
    {
        printf("Enter an arithmetic expression or 'quit' to exit: ");
        fgets(expression, sizeof(expression), stdin);
        expression[strcspn(expression, "\n")] = '\0'; // remove newline character

        if (strcmp(expression, "quit") == 0)
            break;

        // check if the expression starts with "STO(" and ends with ")"
        if (strncmp(expression, "STO(", 4) == 0 && expression[strlen(expression) - 1] == ')')
        {
            // extract the variable name from the expression
            char variable = expression[4];

            // store the previous result in the specified variable
            stored_variables[variable - 'A'] = prev_result;
            printf("Stored %.2lf in variable %c\n", prev_result, variable);
        }
        else
        {
            double result;
            int status = eval(expression, &result, &prev_result, stored_variables);

            if (status == SUCCESS)
                printf("Result: %.2lf\n", result);
            else
            {
                printf("Error: ");
                switch (status)
                {
                case ERROR_DIVISION_BY_ZERO:
                    printf("Division by zero\n");
                    break;
                case ERROR_UNKNOWN_FUNCTION:
                    printf("Unknown function\n");
                    break;
                case ERROR_UNEXPECTED_CHARACTER:
                    printf("Unexpected character\n");
                    break;
                case ERROR_INCOMPLETE_EXPRESSION:
                    printf("Incomplete expression\n");
                    break;
                default:
                    printf("Unknown error\n");
                }
            }
        }
    }

    return 0;
}
